package etape_4

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType

object Schedules extends App with jacop {

  val seriesNumber = 2
  val localsNumber = 2
  val professorsNumber = 5
  val coursesNumber = 5
  val daysNumber = 5
  val hoursNumber = 4
  val slotsNumber = daysNumber * hoursNumber

  val courseIndex = 0; val professorIndex = 1; val localIndex = 2

  val days = List("lu", "ma", "me", "je", "ve")
  val hours = List("8h30-10h30", "10h45-12h45", "13h45-15h45", "16h00-18h00")
  val series = List("Serie 1", "Serie 2")

  val courses = List("vide", "algo", "compta", "math", "asm")
  val coursesOccurences = List(slotsNumber - 3 - 1 - 2 - 3, 3, 1, 2, 3)

  val locals = List("vide", "A17", "A19")

  val professors = List("vide", "Seront", "Grolaux", "Fernee", "Robin")
  // retient les professeurs pour chaque cours
  val professorsCourses = List(0, 1, 2, 3, 1)
  val professorsHours = List(slotsNumber - 3 - 1 - 2 - 3, 3, 1, 2, 3) // /!\ Nombre d'heures PAR SERIE pour la semaine par cours

  // one list per serie, each list contains 20 (one per slots = days and hours) list that contains the course, the professor and the local.
  // the value "0" for the course, professor and local means that the time-slot is empty.
  val dataSeries = for (s <- List.range(0, seriesNumber)) yield for (s <- List.range(0, slotsNumber)) yield List(new IntVar("courses", 0, 4), new IntVar("professors", 0, 4), new IntVar("locals", 0, 2))

  for (s <- dataSeries) {
    // forces each course to appear coursesOccurences(i) times during the week
    for (i <- List.range(1, coursesNumber)) {
      count(s.map(li => li(courseIndex)), i) #= coursesOccurences(i)
    }
    // forces each course to appear professorsHours(i) times during the week
    for (i <- List.range(1, professorsNumber)) {
      count(s.map(li => li(professorIndex)), i) #= professorsHours(i)
    }
  }

  // structure pour stocker les soft contraints professorales qu'on minisera par un count sur le true du boolvar
  val softConstraints1 = for (i <- List.range(0, slotsNumber * seriesNumber)) yield new BoolVar("s" + i)
	val softConstraints2 = for (i <- List.range(0, slotsNumber * seriesNumber)) yield new BoolVar("s" + i)
  for (i <- List.range(0, slotsNumber)) {
    // assigns each professor to a course for a timeslot
    for (s <- List.range(0, seriesNumber)) {
      dataSeries(s)(i)(courseIndex) #= dataSeries(s)(i)(professorIndex)
    }

    // meme prof ne peut donner cours a des series differentes au meme moment
    OR(AND(dataSeries(0)(i)(professorIndex) #= 0, dataSeries(1)(i)(professorIndex) #= 0), dataSeries(0)(i)(professorIndex) #\= dataSeries(1)(i)(professorIndex))

    // assigns locals, impossible to assign the same locals twice at the same timeslot
    val b = new BoolVar("b")
    b <=> (dataSeries(0)(i)(courseIndex) #\= 0)
    b <=> AND(dataSeries(0)(i)(localIndex) #\= 0, dataSeries(1)(i)(localIndex) #\= dataSeries(0)(i)(localIndex))
    b <=> AND(dataSeries(0)(i)(localIndex) #\= 0, dataSeries(1)(i)(localIndex) #\= dataSeries(0)(i)(localIndex))
    val c = new BoolVar("c")
    c <=> (dataSeries(1)(i)(courseIndex) #\= 0)
    c <=> AND(dataSeries(1)(i)(localIndex) #\= 0, dataSeries(0)(i)(localIndex) #\= dataSeries(1)(i)(localIndex))

    // Seront donne cours uniquement premiere heure au matin tous les jours de la semaine
    if (i % hoursNumber != 0) {
      softConstraints1(i) <=> (dataSeries(0)(i)(professorIndex) #\= 1)
      softConstraints1(i + 20) <=> (dataSeries(1)(i)(professorIndex) #\= 1)
    }
    // Robin donne cours uniquement premiere heure au matin tous les jours de la semaine
    if (i % hoursNumber != 0) {
      softConstraints2(i) <=> (dataSeries(0)(i)(professorIndex) #\= 4)
      softConstraints2(i + 20) <=> (dataSeries(1)(i)(professorIndex) #\= 4)
    }
    // Grolaux travaille que lundi
    if (i / hoursNumber > 1) {
      dataSeries(0)(i)(professorIndex) #\= 2
      dataSeries(1)(i)(professorIndex) #\= 2
    }
  }

  val dataList = List(dataSeries(0).flatMap(_.toList), dataSeries(1).flatMap(_.toList))
  val select = search_vector(dataList, max_regret, indomain_middle) 
  val cost = count(softConstraints1, 0) + count(softConstraints2, 0) * 10;
  val result = minimize(select, cost, printSolutions)

  def printSolutions(): Unit = {
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

  def getScheduleSerie(serie: Int): List[List[String]] = {
    dataSeries(serie).map(s => List(courses(s(courseIndex).value()), professors(s(professorIndex).value()), locals(s(localIndex).value())))
  }
}