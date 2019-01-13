package scheduler

import data.{JobScheduling, SchedulingInstance}


trait JobScheduler {
  def schedule(instance: SchedulingInstance): JobScheduling
}
