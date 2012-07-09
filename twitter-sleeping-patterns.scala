import java.util.{ Date, Calendar, GregorianCalendar, Locale }
import java.text.SimpleDateFormat

def fetch(screenName: String, pages: Int, maxId: Option[Long] = None): Seq[Date] = {
  val url = "http://api.twitter.com/1/statuses/user_timeline.xml?screen_name="+screenName+"&count=200&trim_user=true"+ (maxId map ("&max_id="+_) getOrElse "")
  println(url)
  val xmlData = io.Source fromURL url mkString
  val tws = xml.XML loadString xmlData

  val formatter = new SimpleDateFormat("E MMM dd HH:mm:ss Z yyyy", new Locale("en"))
  val dates = tws \ "status" \ "created_at" map (_.text) map formatter.parse
  val ids   = tws \ "status" \ "id" map (_.text.toLong)

  if (pages == 1 || ids.isEmpty) dates
  else dates ++ fetch(screenName, pages - 1, Some(ids.min - 1))
}

def getCalendar(d: Date) = {
  val calendar = new GregorianCalendar
  calendar.setTime(d)
  calendar
}

def absDay(d: Date) = {
  val calendar = getCalendar(d)
  calendar.get(Calendar.YEAR) * 365 + calendar.get(Calendar.DAY_OF_YEAR)
}

def hour(d: Date) = getCalendar(d).get(Calendar.HOUR_OF_DAY)

def mapCount(i: Int) = i match {
  case -1          => " " // sleep
  case 0           => "0"
  case i if i <= 9 => i.toString
  case i           => "#"
}

def drawMatrix(matrix: Array[Array[Int]]) =
  for ((row, i) <- matrix.transpose.zipWithIndex)
    println( (row map mapCount mkString "") + (" %02d:00" format i))

def drawMatrixVertically(matrix: Array[Array[Int]]) =
  for ((row, i) <- matrix.zipWithIndex)
    println(row map mapCount mkString "")

// ***

if (args.isEmpty) {
  println("arguments: screen_name pages_to_fetch")
  sys.exit()
}

val pagesToFecth = if (args.size >= 2) args(1).toInt else 5
val dates = fetch(args.head, pagesToFecth)

val max = dates map absDay max
val min = dates map absDay min
val days = max - min + 1

val matrix = Array.ofDim[Int](days, 24)
for (d <- dates; day = absDay(d) - min; h = hour(d)) matrix(day)(h) += 1

var unactiveHours, startIdx = 0
def idx(day: Int, hour: Int) = day * 24 + hour
def dh(idx: Int) = (idx / 24, idx % 24)
for {
  day  <- 0 until matrix.size
  hour <- 0 until 24
} {
  val active = matrix(day)(hour) > 0
  if (!active) {
    if (unactiveHours == 0) { // start of unactive period
      startIdx = idx(day, hour)
    }
    unactiveHours += 1
  } else {
    if (unactiveHours > 6) { // more than six subsequent unactive hours
      for (i <- startIdx until idx(day, hour); (d, h) = dh(i))
        matrix(d)(h) = -1
    }
    unactiveHours = 0
  }
}

matrix grouped 250 foreach { m => drawMatrix(m); println() }
