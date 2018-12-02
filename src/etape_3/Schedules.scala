package etape_3

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType

object Schedules extends jacop {

  val seriesNumber = 2
  val localsNumber = 2
  val professorsNumber = 4
  val coursesNumber = 4
  val daysNumber = 5
  val hoursNumber = 4
  val slotsNumber = daysNumber * hoursNumber

  val courseIndex = 0; val professorIndex = 1; val localIndex = 2

  val days = List("lu", "ma", "me", "je", "ve")
  val hours = List("8h30-10h30", "10h45-12h45", "13h45-15h45", "16h00-18h00")
  val series = List("Serie 1", "Serie 2")
  val courses = List("vide", "algo", "compta", "math", "asm")
  val coursesOccurences = List(slotsNumber - 2 - 1 - 2 - 1, 2, 1, 2, 1)
  val locals = List("vide", "A17", "A19")
  val professors = List("vide", "Seront", "Grolaux", "Fernee", "Robin")

  // one list per serie, each list contains 20 (one per slots = days and hours) list that contains the course, the professor and the local.
  // the value "0" for the course, professor and local means that the time-slot is empty.
  val dataSeries = for (s <- List.range(0, seriesNumber)) yield for (s <- List.range(0, slotsNumber)) yield List(new IntVar("courses", 0, 4), new IntVar("professors", 0, 4), new IntVar("locals", 0, 2))
  
  for (s <- dataSeries) {
    // forces each course to appear coursesOccurences(i) times during the week
    for (i <- List.range(0, coursesNumber)) {
      count(s.map((li) => li(courseIndex)), i) #= coursesOccurences(i)
    }
  }

  for (i <- List.range(0, slotsNumber)) {
    // assigns each professor that a course for a timeslot
    dataSeries(0)(i)(courseIndex) #= dataSeries(0)(i)(professorIndex)
    dataSeries(1)(i)(courseIndex) #= dataSeries(1)(i)(professorIndex)
    OR(AND(dataSeries(0)(i)(professorIndex) #= 0, dataSeries(1)(i)(professorIndex) #= 0), dataSeries(0)(i)(professorIndex) #\= dataSeries(1)(i)(professorIndex))

    // assigns locals, impossible to assign the same locals twice at the same timeslot
    val b = new BoolVar("b")
    b <=> (dataSeries(0)(i)(courseIndex) #\= 0)
    b <=> AND(dataSeries(0)(i)(localIndex) #\= 0, dataSeries(1)(i)(localIndex) #\= dataSeries(0)(i)(localIndex))
    val c = new BoolVar("c")
    c <=> (dataSeries(1)(i)(courseIndex) #\= 0)
    c <=> AND(dataSeries(1)(i)(localIndex) #\= 0, dataSeries(0)(i)(localIndex) #\= dataSeries(1)(i)(localIndex))

    // Seront donne pas cours premiere heure au matin tous les jours de la semaine
    if (i % hoursNumber == 0) {
      dataSeries(0)(i)(professorIndex) #\= 1
      dataSeries(1)(i)(professorIndex) #\= 1
    }
    // Grolaux ne donne pas cours le jeudi
    if (i / hoursNumber == 3) {
      dataSeries(0)(i)(professorIndex) #\= 2
      dataSeries(1)(i)(professorIndex) #\= 2
    }
    // Grolaux ne donne pas cours le vendredi
    if (i / hoursNumber == 4) {
      dataSeries(0)(i)(professorIndex) #\= 2
      dataSeries(1)(i)(professorIndex) #\= 2
    }
  }

  def printSolutions(): Unit = {
    //dataSeries.foreach(s => s.)
    for (s <- List.range(0, seriesNumber)) {
      println(series(s))
      for (h <- List.range(0, hoursNumber)) {
        print(hours(h))
        for (d <- List.range(0, daysNumber)) {
          val slot = hoursNumber * d + h;
          print("\t" + courses(dataSeries(s)(slot)(courseIndex).value()))
          print("\t" + professors(dataSeries(s)(slot)(professorIndex).value()))
          print("\t" + locals(dataSeries(s)(slot)(localIndex).value()) + "\t");
        }
        println("\n")
      }
    }
  }

  val result = satisfy(search(dataSeries(0).flatMap(_.toList) ++ dataSeries(1).flatMap(_.toList), input_order, indomain_min), printSolutions)

  def getScheduleSerie(serie: Int): List[List[String]] = {
    dataSeries(serie).map(s => List(courses(s(courseIndex).value()), professors(s(professorIndex).value()), locals(s(localIndex).value())))
  }
}