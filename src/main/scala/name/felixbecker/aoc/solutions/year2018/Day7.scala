package name.felixbecker.aoc.solutions.year2018

import name.felixbecker.aoc.solutions.year2018.Day7.BuildStepId

import scala.annotation.tailrec
import scala.io.Source

case class BuildStep(id: BuildStepId, mustRunAfter: Set[BuildStepId])

object Day7 extends App {

  val fixedWorkTimeOffset = 60 // in sample 0

  type BuildStepId = Char

  val buildSteps = Source.fromResource("2018/day7/input.txt").getLines().foldLeft[Map[BuildStepId, BuildStep]](Map.empty) { case (map, rule) =>
    val dependsOn: BuildStepId = rule.charAt(5)
    val stepName: BuildStepId = rule.charAt(36)
    val mapWithStep = map.get(stepName) match {
      case None => map + (stepName -> BuildStep(stepName, Set(dependsOn)))
      case Some(bs) => map + (stepName -> bs.copy(mustRunAfter = bs.mustRunAfter + dependsOn))
    }

    if(mapWithStep.contains(dependsOn)) {
      mapWithStep
    } else {
      mapWithStep + (dependsOn -> BuildStep(dependsOn, Set.empty))
    }
  }

  type SecondsTaken = Int
  type WorkerId = Int
  type Seconds = Int

  case class WorkResult(stepOrder: List[BuildStepId], secondsTaken: SecondsTaken)
  case class BusyWorker(buildStepId: BuildStepId, timeLeft: Seconds)
  @tailrec
  def solveSteps(openSteps: Map[BuildStepId, BuildStep],
                 orderedStepsDone: List[BuildStepId] = Nil,
                 parallelWorkers: Int = 1,
                 busyWorkers: Set[BusyWorker] = Set.empty,
                 secondsTaken: SecondsTaken = 0
                ): WorkResult = {

    if(openSteps.isEmpty){
      WorkResult(orderedStepsDone.reverse, secondsTaken)
    } else {

      val possibleNextSteps = openSteps.values.filter { bs =>
        bs.mustRunAfter.count(afterStepId => openSteps.contains(afterStepId)) == 0 &&
          busyWorkers.count(bw => bw.buildStepId == bs.id) == 0
      }.toList.sortBy(_.id)

      if(busyWorkers.size == parallelWorkers || possibleNextSteps.isEmpty){

        val nextFinish = busyWorkers.map(_.timeLeft).min
        val nextFinishedBuildSteps = busyWorkers.filter(_.timeLeft == nextFinish).map(_.buildStepId)

        solveSteps(
          openSteps.filterKeys(k => !nextFinishedBuildSteps.contains(k)),
          nextFinishedBuildSteps.toList.sorted ::: orderedStepsDone,
          parallelWorkers,
          busyWorkers.filterNot(_.timeLeft == nextFinish).map(remaining => remaining.copy(timeLeft = remaining.timeLeft - nextFinish)),
          secondsTaken + nextFinish
        )
      } else {
        val availableWorkers = parallelWorkers - busyWorkers.size
        val nextSteps = possibleNextSteps.take(availableWorkers)
        val newBusyWorkers = busyWorkers ++ nextSteps.map(ns => BusyWorker(ns.id, ns.id - 'A' + fixedWorkTimeOffset + 1)).toSet
        solveSteps(openSteps, orderedStepsDone, parallelWorkers, newBusyWorkers, secondsTaken)
      }
    }
  }

  val result1 = solveSteps(buildSteps)
  println(s"Solution 1: ${result1.stepOrder.mkString} - Time taken: ${result1.secondsTaken}")

  val result2 = solveSteps(buildSteps, parallelWorkers = 5)
  println(s"Solution 2: ${result2.stepOrder.mkString} - Time taken: ${result2.secondsTaken}")

}
