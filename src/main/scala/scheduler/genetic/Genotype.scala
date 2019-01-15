package scheduler.genetic

import java.util.concurrent.ThreadLocalRandom

import scala.collection.immutable.BitSet.BitSetN
import scala.collection.{BitSet, mutable}

class Genotype(val mask: BitSet, var momentum: Int = -1) {
  var penalty: Int = -1
  var midJobId: Int = -1
  var ttl: Int = 3

  def apply(penalty: Int, midJobId: Int): Unit = {
    this.penalty = penalty
    this.midJobId = midJobId

    if (penalty != Int.MaxValue)
      this.momentum = -1
  }

  def increaseMomentum(): Unit = {
    momentum = (momentum * 1.05).toInt
  }

  def increaseAge(): Unit = {
    ttl -= 1
  }

  def invalid(): Boolean = !valid()
  def valid(): Boolean = momentum < 0
  def notCalculated(): Boolean = penalty < 0
  def tooOld(): Boolean = ttl <= 0
}

object Genotype {
  def cross(first: Genotype, second: Genotype, n: Int): Genotype = {
    val selection = randomMask(n)
    val newMask = (first.mask & selection) | (second.mask &~ selection)

    val momentum = Array(first, second).map(gen => if (gen.valid()) gen.penalty else gen.momentum).sum / 2

    new Genotype(newMask, momentum)
  }

  def mutate(gen: Genotype, n: Int): Genotype = {
    val rand = ThreadLocalRandom.current
    val mutPos = rand.nextInt(n)

    val tempMask = new mutable.BitSet(gen.mask.toBitMask)

    tempMask(mutPos) = !tempMask(mutPos)
    new Genotype(new BitSetN(tempMask.toBitMask), gen.momentum)
  }

  def same(first: Genotype, second: Genotype): Boolean = (first.mask ^ second.mask).isEmpty

  private def randomMask(n: Int): BitSet = {
    val rand = ThreadLocalRandom.current
    val maskSize = (n.toDouble / 64).ceil.toInt
    val elems = (0 to maskSize).map(_ => rand.nextLong).toArray

    new BitSetN(elems)
  }
}