import scala.io.StdIn.readLine
import scala.util.Try

@main
def main(): Unit = {
  val studentInfo = getStudentInfoWithRetry()
  printStudentRecord(studentInfo)
}

def getStudentInfo(): (String, Int, Int, Double, Char) = {
  val name = readLine("Enter Student's Name : ").trim
  val marks = readLine("Enter Marks Obtained : ").trim.toInt
  val totalMarks = readLine("Enter Total Possible Marks : ").trim.toInt

  val percentage = (marks.toDouble / totalMarks) * 100
  val grade = percentage match {
    case x if x >= 90 => 'A'
    case x if x >= 75 => 'B'
    case x if x >= 50 => 'C'
    case _            => 'D'
  }

  (name, marks, totalMarks, percentage, grade)
}

def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
  val (name, marks, totalMarks, percentage, grade) = record
  println(s"Name: $name")
  println(s"Marks: $marks")
  println(s"Total Marks: $totalMarks")
  println(f"Percentage: $percentage%.2f%%")
  println(s"Grade: $grade")
}

def validateInput(
    name: String,
    marks: String,
    totalMarks: String
): (Boolean, Option[String]) = {
  if (name.isEmpty) {
    (false, Some("Name Cannot be Empty!"))
  } else if (!Try(marks.toInt).isSuccess || marks.toInt < 0) {
    (false, Some("Marks Must be a Positive Integer!"))
  } else if (
    !Try(totalMarks.toInt).isSuccess || totalMarks.toInt < marks.toInt
  ) {
    (
      false,
      Some(
        "Total Marks Must be a Positive Integer Greater Than or Equal to Marks Obtained!"
      )
    )
  } else {
    (true, None)
  }
}

def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
  var isValid = false
  var name = ""
  var marks = ""
  var totalMarks = ""
  var errorMessage: Option[String] = None

  while (!isValid) {
    name = readLine("Enter Student's Name : ").trim
    marks = readLine("Enter Marks Obtained : ").trim
    totalMarks = readLine("Enter Total Possible Marks : ").trim

    val validationResult = validateInput(name, marks, totalMarks)
    isValid = validationResult._1 // Accessing the 1st element of the tuple
    errorMessage = validationResult._2

    if (!isValid) {
      println(s"Error: ${errorMessage.getOrElse("Invalid Input!")}")
    }
  }

  val marksInt = marks.toInt
  val totalMarksInt = totalMarks.toInt
  val percentage = (marksInt.toDouble / totalMarksInt) * 100
  val grade = percentage match {
    case x if x >= 90 => 'A'
    case x if x >= 75 => 'B'
    case x if x >= 50 => 'C'
    case _            => 'D'
  }

  (name, marksInt, totalMarksInt, percentage, grade)
}
