package scheduler.genetic

import java.util.concurrent.ThreadLocalRandom

import data.loader.UpperBoundsLoader
import data.{JobScheduling, ScheduledJob, SchedulingInstance}
import scheduler.BaseOffsetScheduler

import scala.collection.immutable.BitSet.BitSetN
import scala.collection.mutable.ArrayBuffer
import scala.collection.{BitSet, mutable}

class GeneticJobScheduler(val populationSize: Int = 500, val execMillis: Long = 30 * 1000) extends BaseOffsetScheduler {
  private val upperBounds = new UpperBoundsLoader

  private var aOrdered = Array.empty[ScheduledJob]
  private var bOrdered = Array.empty[ScheduledJob]
  private var jobPos = Array.empty[Int]

  override def schedule(instance: SchedulingInstance): JobScheduling = {
    makeIndexing(instance)

    val n = instance.jobs.length
    val d = instance.getDeadline

    if (n > 20) geneticSearch(instance, n, d) else fullSearch(instance, n, d)
  }

  private def geneticSearch(instance: SchedulingInstance, n: Int, deadline: Int): JobScheduling = {
    val upperBound = upperBounds.getUpperBound(instance)
    val progress = new GenerationProgress(instance, upperBound, execMillis)

    val startMillis = System.currentTimeMillis()

    var population = initPopulation(instance, n, deadline)
    var legend = population.head

    var millis = 0L
    var generation = 0

    calculatePenalties(population, deadline)

    progress.update(generation, legend.penalty, 0, population.length)
    progress.display()

    while (millis < execMillis) {
      population = replication(population, n)

      calculatePenalties(population, deadline)
      population = limitPopulation(population)
      populationAging(population)

      val leader = population.minBy(_.penalty)
      if (leader.penalty < legend.penalty)
        legend = leader

      millis = System.currentTimeMillis() - startMillis
      generation += 1

      progress.update(generation, leader.penalty, millis, population.length)
      progress.display()
    }

    progress.finish()

    val orderedJobs = orderJobs(legend.mask, legend.midJobId)
    makeSchedulingWithOffset(instance, orderedJobs)
  }

  private def replication(population: Array[Genotype], n: Int): Array[Genotype] = {
    val crossed = crossover(population, n)
    val mutated = mutation(population, n)

    population ++ crossed ++ mutated
  }

  private def crossover(population: Array[Genotype], n: Int): Array[Genotype] = {
    val ordered = population.sortBy(gen => (gen.penalty, gen.momentum))

    ordered.zipWithIndex.drop(1).par.map { case (gen, i) =>
      val rand = ThreadLocalRandom.current
      val withPos = rand.nextInt(i)

      Genotype.cross(ordered(withPos), gen, n)
    }.toArray
  }

  private def mutation(population: Array[Genotype], n: Int): Array[Genotype] = {
    population.par.map(gen => Genotype.mutate(gen, n)).toArray
  }

  private def calculatePenalties(population: Array[Genotype], deadline: Int): Unit = {
    population.filter(_.notCalculated()).par.foreach { gen =>
      val (pen, mid) = calculatePenalty(gen.mask, deadline)
      gen.apply(pen, mid)
    }
  }

  private def limitPopulation(newPopulation: Array[Genotype]): Array[Genotype] = {
    val population = newPopulation.filter(!_.tooOld())
    val valid = withoutDuplicates(population.filter(_.valid()))
    val invalid = population.filter(_.invalid())

    valid.take(populationSize) ++ invalid.take(populationSize)
  }

  private def withoutDuplicates(population: Array[Genotype]): Array[Genotype] = {
    val groups = population.sortBy(gen => (gen.penalty, gen.ttl)).groupBy(gen => gen.penalty)

    groups.par.map { case (pen, gens) =>
      gens.zipWithIndex.filterNot { case (gen, i) =>
        (0 until i).map(j => gens(j)).exists(pgen => Genotype.same(gen, pgen))
      }.map(_._1)
    }.toArray.flatten
  }

  private def populationAging(population: Array[Genotype]): Unit = {
    population.filter(_.invalid()).foreach(_.increaseMomentum())
    population.foreach(_.increaseAge())
  }

  private def initPopulation(instance: SchedulingInstance, n: Int, d: Int): Array[Genotype] = {
    val mask = new mutable.BitSet(n)
    val abOrdered = instance.jobs.sortBy(job => job.a - job.b)
    var length = 0

    abOrdered.foreach { job =>
      if (length + job.p <= d) {
        val pos = jobPos(job.id)

        mask(pos) = true
        length += job.p
      }
    }

    val population = new ArrayBuffer[Genotype]
    population.append(new Genotype(new BitSetN(mask.toBitMask)))

    abOrdered.reverse.foreach { job =>
      val pos = jobPos(job.id)

      if (mask(pos)) {
        mask(pos) = false
        population.append(new Genotype(new BitSetN(mask.toBitMask)))
      }
    }

    population.toArray
  }

  private def fullSearch(instance: SchedulingInstance, n: Int, d: Int): JobScheduling = {
    var bestMask = 0L
    var (bestPenalty, midJob) = calculatePenalty(constructMask(0L, n), d)

    (1L until (1L << n)).par.foreach { mask =>
      val (penalty, mJob) = calculatePenalty(constructMask(mask, n), d)

      if (penalty < bestPenalty) {
        synchronized {
          if (penalty < bestPenalty) {
            bestMask = mask
            bestPenalty = penalty
            midJob = mJob
          }
        }
      }
    }

    val orderedJobs = orderJobs(constructMask(bestMask, n), midJob)
    makeSchedulingWithOffset(instance, orderedJobs)
  }

  private def constructMask(mask: Long, bitsNo: Int): BitSet = {
    new BitSetN(Array(mask))
  }

  private def calculatePenalty(mask: BitSet, deadline: Int): (Int, Int) = {
    var aTime = 0
    var aPenalty = 0
    var aWeights = 0
    var bTime = 0
    var bPenalty = 0
    var bWeights = 0

    var midJobId = -1

    aOrdered foreach { job =>
      val pos = jobPos(job.id)

      if (mask(pos)) {
        aPenalty += job.a * aTime
        aTime += job.p
        aWeights += job.a
      }
    }

    if (aTime > deadline)
      return (Int.MaxValue, midJobId)

    val afterJobs = bOrdered.filter(job => !mask(jobPos(job.id)))

    afterJobs foreach { job =>
      bTime += job.p
      bPenalty += job.b * bTime
      bWeights += job.b
    }

    val wDiff = bWeights - aWeights
    var penalty = aPenalty + bPenalty
    val margin = deadline - aTime
    var seenWeights = 0
    var seenLength = 0

    if (wDiff < 0) {
      return (penalty, midJobId)
    }

    afterJobs foreach {job =>
      val shift = math.min(margin, job.p)
      val newPenalty = aPenalty + bPenalty + job.p * seenWeights - (job.b * seenLength) - wDiff * shift

      if (newPenalty < penalty) {
        penalty = newPenalty
        midJobId = job.id
      }

      seenWeights += job.b
      seenLength += job.p
    }

    (penalty, midJobId)
  }

  private def orderJobs(mask: BitSet, midJobId: Int): Array[ScheduledJob] = {
    val beforeJobs = aOrdered.filter(job => mask(jobPos(job.id))).reverse
    val afterJobs = bOrdered.filter(job => !(job.id == midJobId || mask(jobPos(job.id))))

    val middleJob = if (midJobId >= 0) Array(bOrdered.find(job => job.id == midJobId).get) else Array.empty[ScheduledJob]

    beforeJobs ++ middleJob ++ afterJobs
  }

  private def makeIndexing(instance: SchedulingInstance): Unit = {
    aOrdered = instance.jobs.sortBy(job => job.p.toDouble / job.a)
    bOrdered = instance.jobs.sortBy(job => job.p.toDouble / job.b)
    jobPos = new Array(instance.jobs.length)

    instance.jobs.zipWithIndex.foreach {
      case (job, pos) => jobPos(job.id) = pos
    }
  }
}
