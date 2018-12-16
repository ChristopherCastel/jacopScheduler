package etape_5

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType

object Schedules extends App with jacop {

  val blocsNumber = 2
  val seriesNumber = List(1, 1, 1)
  
  val localsNumber = 13
  val professorsNumber = List(4, 4, 4)
  val coursesNumber = List(4, 4, 4)
  val daysNumber = 3
  val hoursNumber = 4
  val slotsNumber = daysNumber * hoursNumber

  val courseIndex = 0; val professorIndex = 1; val localIndex = 2

  val days = List("lu", "ma", "me", "je", "ve")
  val hours = List("8h30-10h30", "10h45-12h45", "13h45-15h45", "16h00-18h00")
  val series = for (b <- List.range(0, blocsNumber)) yield for (s <- List.range(0, seriesNumber(b))) yield "Bloc " + (b + 1) + " Serie " + (s + 1)

  //val courses = List("vide", "algo th", "asm", "algo ex", "anglais")
  //val coursesThex = List("vide", "TH", "TH", "EX", "EX")
  //val coursesOccurences = List(-1, 1, 1, 2, 2)
  val courses = List(List("vide", "algo th", "asm", "algo ex", "anglais"), List("vide", "Unix th", "OS", "Unix ex", "Android"), List("vide", "Infra th", "Ethique", "Infra ex", "pattern"))
  val coursesThex = List(List("vide", "TH", "TH", "EX", "EX"), List("vide", "TH", "TH", "EX", "EX"), List("vide", "TH", "TH", "EX", "EX"))
  val coursesOccurences = List(List(-1, 1, 1, 2, 2), List(-1, 1, 1, 2, 2), List(-1, 1, 1, 2, 2))
  
  
  val locals = List("vide", "Aud A", "Aud B", "B11", "B12", "B21", "A017", "A019", "A025", "A026", "B22", "B25", "D3", "LL")
  val localsThex = List("vide", "TH", "TH", "TH", "TH", "TH", "EX", "EX", "EX", "EX", "EX", "EX", "EX", "EX") //
  val localsCapacity = List(0, 4, 4, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1) // Nombre Max de séries d'un bloc qu'un local peut tenir

  val professors = List(List("vide", "Seront", "Grolaux", "Fernee", "Robin"), List("vide", "Ninane", "???", "VdMln", "Leconte"), List("vide", "Choquet", "Gwenael", "???bis", "Damas"))
  // retient les professeurs pour chaque cours
  //val professorsCourses = List((0, 1, 2, 3, 1), (0, 1, 2, 3, 1), (0, 1, 2, 3, 1))
  val professorsHours = List(List(-1, 1, 1, 2, 2), List(-1, 1, 1, 2, 2), List(-1, 1, 1, 2, 2)) // /!\ Nombre d'heures PAR SERIE pour la semaine par cours

  // one list per serie, each list contains 20 (one per slots = days and hours) list that contains the course, the professor and the local.
  // the value "0" for the course, professor and local means that the time-slot is empty.
  val dataSeries = for (b <- List.range(0, blocsNumber)) yield for (s <- List.range(0, seriesNumber(b))) yield for (s <- List.range(0, slotsNumber)) yield List(new IntVar("courses", 0, coursesNumber(b)), new IntVar("professors", 0, professorsNumber(b)), new IntVar("locals", 0, localsNumber))

  // soft constraints
  val softConstraintsSeront = for (b <- List.range(0, blocsNumber)) yield for (se <- List.range(0, seriesNumber(b))) yield for (sl <- List.range(0, slotsNumber)) yield new BoolVar("b" + b + "se" + se + "sl" + sl)
  val softConstraintsRobin = for (b <- List.range(0, blocsNumber)) yield for (se <- List.range(0, seriesNumber(b))) yield for (sl <- List.range(0, slotsNumber)) yield new BoolVar("b" + b + "se" + se + "sl" + sl)
  val softConstraintsFerneeuw = for (b <- List.range(0, blocsNumber)) yield for (se <- List.range(0, seriesNumber(b))) yield for (sl <- List.range(0, slotsNumber)) yield new BoolVar("b" + b + "se" + se + "sl" + sl)

  //courseBeforeAnother(1, 3) // "algo th" before "algo ex" 
  manageOccurences()
  assignEachProfessorToACourse()
  assignEachCourseToALocal()
  manageTimeInconsistencyBetweenBlocs()
  manageTimeInconsistencyBetweenSeriesOfBloc()
  theoricalCoursesInTheoricalLocal()
  exerciceCoursesInExerciceLocal()
  courseToLocal(4, 13) // english class given in the local "labo langue"
  hardProfessorNotWorkingDay(2, 1) // Grolaux doesn't work on monday
  hardProfessorNotWorkingDay(2, 2) // Grolaux doesn't work on wed
  hardProfessorNotWorkingHour(2, 0) // Grolaux doesn't work the first hour
  hardProfessorNotWorkingHour(2, 1) // Grolaux doesn't work the second hour
  //softProfessorNotWorkingHour()
  
  def softProfessorNotWorkingHour() {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc))) {
      for (iSlot <- List.range(0, slotsNumber) if iSlot % hoursNumber != 0) {
        // Seront donne cours uniquement premiere heure au matin tous les jours de la semaine
        softConstraintsSeront(iBloc)(iSerie)(iSlot) <=> (dataSeries(iBloc)(iSerie)(iSlot)(professorIndex) #\= 1)
        // Robin donne cours uniquement premiere heure au matin tous les jours de la semaine
        softConstraintsRobin(iBloc)(iSerie)(iSlot) <=> (dataSeries(iBloc)(iSerie)(iSlot)(professorIndex) #\= 4)
        // Ferneeuw donne cours uniquement premiere heure au matin tous les jours de la semaine
        softConstraintsFerneeuw(iBloc)(iSerie)(iSlot) <=> (dataSeries(iBloc)(iSerie)(iSlot)(professorIndex) #\= 3)
      }
    }
  }

  def manageOccurences() {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc))) {
      for (i <- List.range(1, coursesNumber(iBloc) + 1)) {
        // retrieves all course's intvar
        val coursesIntvar = dataSeries(iBloc)(iSerie).map(slot => slot(courseIndex))
        // forces each course to appear coursesOccurences(i) times during the week
        count(coursesIntvar, i) #= coursesOccurences(iBloc)(i)

        val professorsIntvar = dataSeries(iBloc)(iSerie).map(slot => slot(professorIndex))
        // forces each course to appear professorsHours(i) times during the week
        count(professorsIntvar, i) #= professorsHours(iBloc)(i)
      }
    }
  }

  def assignEachProfessorToACourse() {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc)); iSlot <- List.range(0, slotsNumber)) {
      // professor and its course are linked by their index
      dataSeries(iBloc)(iSerie)(iSlot)(courseIndex) #= dataSeries(iBloc)(iSerie)(iSlot)(professorIndex)
    }
  }

  def assignEachCourseToALocal() {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc)); iSlot <- List.range(0, slotsNumber)) {
      // Either the course is "empty" and the local is "empty", or the course and the local aren't "empty"
      OR(AND(dataSeries(iBloc)(iSerie)(iSlot)(courseIndex) #\= 0, dataSeries(iBloc)(iSerie)(iSlot)(localIndex) #\= 0), AND(dataSeries(iBloc)(iSerie)(iSlot)(courseIndex) #= 0, dataSeries(iBloc)(iSerie)(iSlot)(localIndex) #= 0))
    }
  }

  def courseToLocal(iCourse: Int, iLocal: Int) {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc)); iSlot <- List.range(0, slotsNumber)) {
      // Either the course is constrained to the given local, or the course is not the one given
      OR(AND(dataSeries(iBloc)(iSerie)(iSlot)(courseIndex) #= iCourse, dataSeries(iBloc)(iSerie)(iSlot)(localIndex) #= iLocal), dataSeries(iBloc)(iSerie)(iSlot)(courseIndex) #\= iCourse)
    }
  }

  def manageTimeInconsistencyBetweenBlocs() {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc)); iSlot <- List.range(0, slotsNumber)) {
      for (iBlocBis <- List.range(0, blocsNumber) if iBlocBis != iBloc) {
          for (iSerieBis <- List.range(0, seriesNumber(iBlocBis))) {
              // meme prof ne peut donner cours a des series de blocs differentes au meme moment
            OR(AND(dataSeries(iBloc)(iSerie)(iSlot)(professorIndex) #= 0, dataSeries(iBlocBis)(iSerieBis)(iSlot)(professorIndex) #= 0), dataSeries(iBloc)(iSerie)(iSlot)(professorIndex) #\= dataSeries(iBlocBis)(iSerieBis)(iSlot)(professorIndex))
            // meme local ne peut etre utilise en meme temps par des series de blocs differents
            OR(AND(dataSeries(iBloc)(iSerie)(iSlot)(localIndex) #= 0, dataSeries(iBlocBis)(iSerieBis)(iSlot)(localIndex) #= 0), dataSeries(iBloc)(iSerie)(iSlot)(localIndex) #\= dataSeries(iBlocBis)(iSerieBis)(iSlot)(localIndex))
          }
        }
    }
  }

  def manageTimeInconsistencyBetweenSeriesOfBloc() {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc)); iSlot <- List.range(0, slotsNumber)) {
      for (iLocal <- List.range(1, localsNumber + 1)) {
        val amountSeries = for (s <- List.range(0, seriesNumber(iBloc))) yield new BoolVar("s" + s)
        for (iSerieBis <- List.range(0, seriesNumber(iBloc))) {
          amountSeries(iSerieBis) <=> (dataSeries(iBloc)(iSerieBis)(iSlot)(localIndex) #= iLocal)
        }
        // Forces a local to have max localsCapacity(local) of series within a bloc
        count(amountSeries, 1) #<= localsCapacity(iLocal)
      }
      for (iSerieBis <- List.range(0, seriesNumber(iBloc)) if iSerieBis != iSerie) {
        OR(
          // Either the course is "empty" and the local is "empty"
          AND(dataSeries(iBloc)(iSerie)(iSlot)(professorIndex) #= 0, dataSeries(iBloc)(iSerieBis)(iSlot)(professorIndex) #= 0),
          // Either the teachers and locals of the two series are different, Or the locals are the same (and thus so are the teachers)
          OR(
            AND(dataSeries(iBloc)(iSerie)(iSlot)(professorIndex) #\= dataSeries(iBloc)(iSerieBis)(iSlot)(professorIndex),dataSeries(iBloc)(iSerie)(iSlot)(localIndex) #\= dataSeries(iBloc)(iSerieBis)(iSlot)(localIndex)),
            dataSeries(iBloc)(iSerie)(iSlot)(localIndex) #= dataSeries(iBloc)(iSerieBis)(iSlot)(localIndex)))
      }
    }
  }

  def theoricalCoursesInTheoricalLocal() {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc)); iSlot <- List.range(0, slotsNumber)) {
      // if course = theorical then local = theorical
      val sizeTh = localsThex.count(c => c.equals("TH"))
      for (i <- List.range(1, coursesNumber(iBloc) + 1) if coursesThex(iBloc)(i).equals("TH")) {
        OR(dataSeries(iBloc)(iSerie)(iSlot)(courseIndex) #\= i, dataSeries(iBloc)(iSerie)(iSlot)(localIndex) #<= sizeTh)
      }
    }
  }
  
  def exerciceCoursesInExerciceLocal() {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc)); iSlot <- List.range(0, slotsNumber)) {
      // if course = exercices then local = exercices
      val sizeTh = localsThex.count(c => c.equals("TH"))
      for (i <- List.range(1, coursesNumber(iBloc) + 1) if coursesThex(iBloc)(i).equals("EX")) {
        OR(dataSeries(iBloc)(iSerie)(iSlot)(courseIndex) #\= i, dataSeries(iBloc)(iSerie)(iSlot)(localIndex) #> sizeTh)
      }
    }
  }

  def courseBeforeAnother(courseBefore: Int, courseAfter: Int) {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc)); iSlot <- List.range(0, slotsNumber)) {
      val boolvars = for (b <- List.range(0, slotsNumber - iSlot - 1)) yield new BoolVar("b" + b)
      for (iNextSlots <- List.range(iSlot + 1, slotsNumber)) {
        boolvars(iNextSlots - iSlot - 1) <=> (dataSeries(iBloc)(iSerie)(iNextSlots)(courseIndex) #= courseAfter)
      }
      OR(dataSeries(iBloc)(iSerie)(iSlot)(courseIndex) #\= courseBefore, AND(dataSeries(iBloc)(iSerie)(iSlot)(courseIndex) #= courseBefore, count(boolvars, 1) #= coursesOccurences(iBloc)(courseAfter)))
    }
  }

  def hardProfessorNotWorkingDay(iProf: Int, day: Int) {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc))) {
      for (iSlot <- List.range(0, slotsNumber) if iSlot / hoursNumber == day) {
        dataSeries(iBloc)(iSerie)(iSlot)(professorIndex) #\= iProf
      }
    }
  }

  def hardProfessorNotWorkingHour(iProf: Int, hour: Int) {
    for (iBloc <- List.range(0, blocsNumber); iSerie <- List.range(0, seriesNumber(iBloc))) {
      for (iSlot <- List.range(0, slotsNumber) if iSlot % hoursNumber == hour) {
        dataSeries(iBloc)(iSerie)(iSlot)(professorIndex) #\= iProf
      }
    }
  }
  
  
  val dataList = dataSeries.map(b => b.map(s => s.flatMap(_.toList)).flatMap(_.toList)).flatMap(_.toList)
  val select = search(dataList, first_fail, indomain_middle) 
  val costSeront = softConstraintsSeront.flatMap(_.toList).flatMap(_.toList) 
  val costRobin = softConstraintsRobin.flatMap(_.toList).flatMap(_.toList)
  val costFerneeuw = softConstraintsFerneeuw.flatMap(_.toList).flatMap(_.toList)
  val cost = count(costSeront, 0) * 100 + count(costRobin, 0) * 100 + count(costFerneeuw, 0)
  //val result = minimize(select, cost, printSolutions)
  val result = satisfy(select, printSolutions)

  def printSolutions(): Unit = {
    for (b <- List.range(0, blocsNumber)) {
      for (s <- List.range(0, seriesNumber(b))) {
        println(series(b)(s))
        for (h <- List.range(0, hoursNumber)) {
          print(hours(h))
          for (d <- List.range(0, daysNumber)) {
            val slot = hoursNumber * d + h;
            print("\t" + courses(b)(dataSeries(b)(s)(slot)(courseIndex).value()))
            print("\t" + professors(b)(dataSeries(b)(s)(slot)(professorIndex).value()))
            print("\t" + locals(dataSeries(b)(s)(slot)(localIndex).value()) + "\t");
          }
          println("\n")
        }
      }
    }
  }

  //def getScheduleSerie(bloc: Int, serie: Int): List[List[String]] = {
  //  dataSeries(bloc)(serie).map(s => List(courses(s(courseIndex).value()), professors(s(professorIndex).value()), locals(s(localIndex).value())))
  //}
}