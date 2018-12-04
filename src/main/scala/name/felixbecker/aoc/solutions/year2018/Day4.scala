package name.felixbecker.aoc.solutions.year2018

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import scala.collection.mutable
import scala.io.Source


object Day4 extends App {

  case class GuardActivity(dateTime: LocalDateTime, activity: String)

  val dtf = DateTimeFormatter.ofPattern("uuuu-MM-dd kk:mm")

  type GuardId = Int

  case class Sleep(start: LocalDateTime, end: LocalDateTime){
    def sleepMinutes = start.until(end, ChronoUnit.MINUTES)
  }
  var sleepByGuard = collection.mutable.Map[GuardId, collection.mutable.MutableList[Sleep]]()

  val orderedGuardActivities = Source.fromResource("2018/day4/input.txt").getLines().map { str =>
    //[1518-11-05 00:45] falls asleep
    GuardActivity(LocalDateTime.parse(str.substring(1,17), dtf), str.substring(19))
  }.toList.sortWith { (a1, a2) =>
    a1.dateTime.isBefore(a2.dateTime)
  }

  var activeGuard: Option[Int] = None
  var fallsAsleep: Option[LocalDateTime] = None

  orderedGuardActivities.foreach { ga =>
    ga.activity match {
      case str if str.startsWith("Guard #") => activeGuard = Some(str.substring(7).split(" ")(0).toInt)
      case "falls asleep" => fallsAsleep = Some(ga.dateTime)
      case "wakes up" =>
        val newSleep = Sleep(fallsAsleep.get, ga.dateTime)
        sleepByGuard.getOrElseUpdate(activeGuard.get, new mutable.MutableList[Sleep]()) += newSleep
        fallsAsleep = None
    }
  }

  val (mostSleepingGuardId, mostSleepingGuardTime) = sleepByGuard.map { case (guardId, sleeps) =>
    guardId -> sleeps.map(_.sleepMinutes).sum
  }.toList.maxBy(x => x._2)

  val mostSleepingMinute = {
    val minuteMap = collection.mutable.Map[Int, Int]()
    sleepByGuard(mostSleepingGuardId).foreach { s =>
      (0 until s.sleepMinutes.toInt).foreach { plusMin =>
        val newTime = s.start.plusMinutes(plusMin)
        val existingMinuteCount = minuteMap.getOrElseUpdate(newTime.getMinute, 0)
        minuteMap(newTime.getMinute) = existingMinuteCount+1
      }
    }
    minuteMap.maxBy(x => x._2)._1
  }

  // Solution 1: 35623
  println(s"Solution 1: ${mostSleepingGuardId*mostSleepingMinute}")

  // Solution 2:
  case class GuardSleepByMinute(guardId: Int, minute: Int)
  val guardSleepsByMinute: Seq[GuardSleepByMinute] = sleepByGuard.flatMap { case (guardId, sleeps) =>
    sleeps.flatMap { s =>
      (0 until s.sleepMinutes.toInt).map { plusMin =>
        val newTime = s.start.plusMinutes(plusMin)
        GuardSleepByMinute(guardId, newTime.getMinute)
      }
    }
  }.toList

  case class MostSleptMinuteByGuard(guardId: Int, minute: Int, minuteCount: Int)

  val guardIdSet = guardSleepsByMinute.map(_.guardId).toSet

  val solution2 = guardIdSet.flatMap { guardId =>
    guardSleepsByMinute.filter(_.guardId == guardId).groupBy(_.minute).map { case (min, entries) =>
      MostSleptMinuteByGuard(guardId, min, entries.length)
    }
  }.maxBy(_.minuteCount)

  println(s"Solution 2: GuardId: ${solution2.guardId} - Minute: ${solution2.minute} (${solution2.minuteCount} times)")
  println(solution2.guardId*solution2.minute)


}
