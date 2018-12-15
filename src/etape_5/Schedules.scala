package etape_5

import JaCoP.scala._
import scala.reflect.ClassManifestFactory.classType

object Schedules extends App with jacop {

  val blocsNumber = 2
  val seriesNumber = List(1, 1, 1)
  val totalSeries = seriesNumber.foldLeft(0)((h, t) => h + t)
  
  val localsNumber = 13
  val professorsNumber = 4
  val coursesNumber = 4
  val daysNumber = 2
  val hoursNumber = 4
  val slotsNumber = daysNumber * hoursNumber

  val courseIndex = 0; val professorIndex = 1; val localIndex = 2

  val days = List("lu", "ma", "me", "je", "ve")
  val hours = List("8h30-10h30", "10h45-12h45", "13h45-15h45", "16h00-18h00")
  val series = for (b <- List.range(0, blocsNumber)) yield for (s <- List.range(0, seriesNumber(b))) yield "Bloc " + (b + 1) + " Serie " + (s + 1)

  val courses = List("vide", "algo th", "anglais", "algo ex", "asm")
  val coursesThex = List("vide", "TH", "EX", "EX", "EX")
  val coursesOccurences = List(-1, 1, 1, 2, 2)
  val courseEnglish = 2

  val locals = List("vide", "Aud A", "Aud B", "B11", "B12", "B21", "A017", "A019", "A025", "A026", "B22", "B25", "D3", "LL")
  val localsThex = List("vide", "TH", "TH", "TH", "TH", "TH", "EX", "EX", "EX", "EX", "EX", "EX", "EX", "EX") 

  val professors = List("vide", "Seront", "Grolaux", "Fernee", "Robin")
  // retient les professeurs pour chaque cours
  val professorsCourses = List(0, 1, 2, 3, 1)
  val professorsHours = List(-1, 1, 1, 2, 2) // /!\ Nombre d'heures PAR SERIE pour la semaine par cours

  // one list per serie, each list contains 20 (one per slots = days and hours) list that contains the course, the professor and the local.
  // the value "0" for the course, professor and local means that the time-slot is empty.
  val dataSeries = for (b <- List.range(0, blocsNumber)) yield for (s <- List.range(0, seriesNumber(b))) yield for (s <- List.range(0, slotsNumber)) yield List(new IntVar("courses", 0, coursesNumber), new IntVar("professors", 0, professorsNumber), new IntVar("locals", 0, localsNumber))

  // soft constraints
  val softConstraintsSeront = for (b <- List.range(0, blocsNumber)) yield for (se <- List.range(0, seriesNumber(b))) yield for (sl <- List.range(0, slotsNumber)) yield new BoolVar("b" + b + "se" + se + "sl" + sl)
  val softConstraintsRobin = for (b <- List.range(0, blocsNumber)) yield for (se <- List.range(0, seriesNumber(b))) yield for (sl <- List.range(0, slotsNumber)) yield new BoolVar("b" + b + "se" + se + "sl" + sl)
  val softConstraintsFerneeuw = for (b <- List.range(0, blocsNumber)) yield for (se <- List.range(0, seriesNumber(b))) yield for (sl <- List.range(0, slotsNumber)) yield new BoolVar("b" + b + "se" + se + "sl" + sl)

  for (indiceBloc <- List.range(0, blocsNumber)) {
    // hard constraints
    for (iSerie <- List.range(0, seriesNumber(indiceBloc))) {
      for (i <- List.range(1, coursesNumber + 1)) {
    	  // forces each course to appear coursesOccurences(i) times during the week
         count(dataSeries(indiceBloc)(iSerie).map(li => li(courseIndex)), i) #= coursesOccurences(i)
         // SOIT cours = theorie et local = theorie SOIT cours = exercices et local = exercice
        //OR(AND(_,_), AND(_,_))
      }
      // forces each course to appear professorsHours(i) times during the week
      for (i <- List.range(1, professorsNumber + 1)) {
        count(dataSeries(indiceBloc)(iSerie).map(li => li(professorIndex)), i) #= professorsHours(i)
      }
      for (iSlot <- List.range(0, slotsNumber)) {
        // english course in labo langues
        // TODO Demander à Grolaux explication du OR
    	  OR(AND(dataSeries(indiceBloc)(iSerie)(iSlot)(courseIndex) #= courseEnglish, dataSeries(indiceBloc)(iSerie)(iSlot)(localIndex) #= 13), dataSeries(indiceBloc)(iSerie)(iSlot)(courseIndex) #\= courseEnglish)
        // assigns each professor to a course for a timeslot
        for (s <- List.range(0, seriesNumber(indiceBloc))) {
          dataSeries(indiceBloc)(s)(iSlot)(courseIndex) #= dataSeries(indiceBloc)(s)(iSlot)(professorIndex)
        }
        
        for (iBlocBis <- List.range(0, blocsNumber)) {
          for (iSerieBis <- List.range(0, seriesNumber(iBlocBis)) if !(iSerie + " " + indiceBloc).equals((iSerieBis + " " + iBlocBis))) {
        	  // meme prof ne peut donner cours a des series differentes au meme moment
            OR(AND(dataSeries(indiceBloc)(iSerie)(iSlot)(professorIndex) #= 0, dataSeries(iBlocBis)(iSerieBis)(iSlot)(professorIndex) #= 0), dataSeries(indiceBloc)(iSerie)(iSlot)(professorIndex) #\= dataSeries(iBlocBis)(iSerieBis)(iSlot)(professorIndex))
            // meme local ne peut etre utilise en meme temps par des cours differents
            OR(AND(dataSeries(indiceBloc)(iSerie)(iSlot)(localIndex) #= 0, dataSeries(iBlocBis)(iSerieBis)(iSlot)(localIndex) #= 0), dataSeries(indiceBloc)(iSerie)(iSlot)(localIndex) #\= dataSeries(iBlocBis)(iSerieBis)(iSlot)(localIndex))
          }
        }
        // SOIT le cours est vide et donc le local est vide, SOIT le cours n'est pas vide et le local n'est pas vide
        OR(AND(dataSeries(indiceBloc)(iSerie)(iSlot)(courseIndex) #\= 0, dataSeries(indiceBloc)(iSerie)(iSlot)(localIndex) #\= 0), AND(dataSeries(indiceBloc)(iSerie)(iSlot)(courseIndex) #= 0, dataSeries(indiceBloc)(iSerie)(iSlot)(localIndex) #= 0))
      }
    }

    for (iSerie <- List.range(0, seriesNumber(indiceBloc))) {
      // Grolaux travaille pas le lundi
      for (iSlot <- List.range(0, slotsNumber) if iSlot / hoursNumber == 0) {
         dataSeries(indiceBloc)(iSerie)(iSlot)(professorIndex) #\= 2
      }
      // Grolaux travaille que l'aprem
      for (iSlot <- List.range(0, slotsNumber) if iSlot % hoursNumber < 2) {
         dataSeries(indiceBloc)(iSerie)(iSlot)(professorIndex) #\= 2
      }
      for (iSlot <- List.range(0, slotsNumber) if iSlot % hoursNumber != 0) {
        if (iSlot % hoursNumber != 0) {
        	// Seront donne cours uniquement premiere heure au matin tous les jours de la semaine
          softConstraintsSeront(indiceBloc)(iSerie)(iSlot) <=> (dataSeries(indiceBloc)(iSerie)(iSlot)(professorIndex) #\= 1)
          // Robin donne cours uniquement premiere heure au matin tous les jours de la semaine
          softConstraintsRobin(indiceBloc)(iSerie)(iSlot) <=> (dataSeries(indiceBloc)(iSerie)(iSlot)(professorIndex) #\= 4)
          // Ferneeuw donne cours uniquement premiere heure au matin tous les jours de la semaine
          softConstraintsFerneeuw(indiceBloc)(iSerie)(iSlot) <=> (dataSeries(indiceBloc)(iSerie)(iSlot)(professorIndex) #\= 3)
        }
      }
    }
  }

  val dataList = dataSeries.map(b => b.map(s => s.flatMap(_.toList)).flatMap(_.toList)).flatMap(_.toList)
  val select = search(dataList, first_fail, indomain_middle) 
  val costSeront = softConstraintsSeront.flatMap(_.toList).flatMap(_.toList) 
  val costRobin = softConstraintsRobin.flatMap(_.toList).flatMap(_.toList)
  val costFerneeuw = softConstraintsFerneeuw.flatMap(_.toList).flatMap(_.toList)
  val cost = count(costSeront, 0) * 100 + count(costRobin, 0) * 100 + count(costFerneeuw, 0)
  val result = minimize(select, cost, printSolutions)

  def printSolutions(): Unit = {
    for (b <- List.range(0, blocsNumber)){
      for (s <- List.range(0, seriesNumber(b))) {
        println(series(b)(s))
        for (h <- List.range(0, hoursNumber)) {
          print(hours(h))
          for (d <- List.range(0, daysNumber)) {
            val slot = hoursNumber * d + h;
            print("\t" + courses(dataSeries(b)(s)(slot)(courseIndex).value()))
            print("\t" + professors(dataSeries(b)(s)(slot)(professorIndex).value()))
            print("\t" + locals(dataSeries(b)(s)(slot)(localIndex).value()) + "\t");
          }
          println("\n")
        }
    }
    }
  }

//  def getScheduleSerie(serie: Int): List[List[String]] = {
//    dataSeries(serie).map(s => List(courses(s(courseIndex).value()), professors(s(professorIndex).value()), locals(s(localIndex).value())))
//  }
}