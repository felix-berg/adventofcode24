import sttp.client4.quick.*
import sttp.client4.Response

import scala.collection.mutable
import scala.util.matching.Regex

import upickle.default.{ read as readJson }

case class Coordinates(lat: Double, lng: Double)

case class Building(
  id: String,
  coords: Coordinates,
  address: String,
  name: String,
  outline: List[Coordinates]
)

def filterBuildings(arr: List[ujson.Value]): List[ujson.Value] =
  arr.filter(_.obj.get("type") match {
    case Some(t) => t.str == "0" // is a building 
    case None => false
  })

def validateBuildingJson(bld: ujson.Value): Unit = {
  assert(bld.obj.keySet.contains("id"), s"$bld has no id")

  val id = bld.obj("id")
  assert(bld.obj.keySet.contains("names"), s"Building $id: has no 'names'")

  val names = bld.obj("names").arr
  assert(names.size > 0, s"Building $id: 'names' is empty")

  assert(bld.obj.keySet.contains("latitude"), s"Building $id: has no latitude")
  assert(bld.obj.keySet.contains("longitude"), s"Building $id: has no longitude")
  assert(bld.obj.keySet.contains("address"), s"Building $id: has no address")
  assert(bld.obj.keySet.contains("coordinates"), s"Building $id: has no 'coordinates'")

  val coordinates = bld.obj("coordinates").arr
  assert(coordinates.size > 0, s"Building $id: 'coordinates' is empty")
}

def cleanUpAddress(addr: String): String =
  addr.replace(" , ", ", ")


def combineNames(names: List[String]): String = {
  if (names.size <= 1) 
    names.mkString
  else {
    val buildingNumberNameRegex: Regex = """Bygning \d{1,4}""".r
    val (List(bnn), others) = names.partition(buildingNumberNameRegex.matches(_))
    val nameList = others.mkString(", ")
    s"$nameList ($bnn)"
  }
}

def parseBuilding(bld: ujson.Value): Building = {
  validateBuildingJson(bld)
  val obj = bld.obj
  val id = obj("id").str
  val address = cleanUpAddress(obj("address").str.trim)
  val coords = Coordinates(obj("latitude").num, obj("longitude").num)

  val outline = obj("coordinates").arr.toList.map(c => 
    Coordinates(
      c.obj("latitude").num,
      c.obj("longitude").num
    )
  )

  val names = obj("names").arr.toList.map(n => n.obj("name").str.trim)
  val name = combineNames(names)

  Building(id, coords, address, name, outline)
}

case class Bounds(tl: Coordinates, br: Coordinates)
def getBounds(outline: List[Coordinates]): Bounds = {
  assert(outline.nonEmpty) 
  val (lats, lngs) = outline.map(c => (c.lat, c.lng)).unzip
  Bounds(
    Coordinates(lats.min, lngs.min),
    Coordinates(lats.max, lngs.max)
  )
}

def normalize(outline: List[Coordinates]): List[Coordinates] = {
  val Bounds(tl, br) = getBounds(outline)
  val scale = Math.max(br.lat - tl.lat, br.lng - tl.lng)
  outline.map(crd => Coordinates(
    (crd.lat - tl.lat) / scale,
    (crd.lng - tl.lng) / scale,
  ))
}

def drawGrid(outline: List[Coordinates], n: Int): String = {
  val ol = normalize(outline)
  val arr = Array.ofDim[Char](n, n)
  for (i <- 0 until n; j <- 0 until n)
    arr(i)(j) = ' '

  var cnt = 0
  for (crd <- ol) {
    val (i, j) = (Math.round(crd.lat * (n-1)).toInt, Math.round(crd.lng * (n-1)).toInt)
    arr(i)(j) = (cnt % 10).toString.head
    cnt += 1
  }

  arr.map(subarr => subarr.mkString)
    .dropWhile(_.forall(c => c == ' '))
    .reverse
    .dropWhile(_.forall(c => c == ' '))
    .reverse
    .mkString("\n")
}


object Main {
  def main(args: Array[String]): Unit = {
    val res = quickRequest
      .get(uri"https://aufind.au.dk/web/xmltojson.php?mode=json")
      .send()

    val arr = ujson.read(res.body).obj("locations").arr
    val buildings = filterBuildings(arr.toList).map(parseBuilding)
    for (b <- buildings) {
      val round = (d: Double) => (d * 1000.0).toInt / 1000.0
      val coordStr = (c: Coordinates) => s"(${round(c.lat)}, ${round(c.lng)})"
      println(
        s"""|#${b.id}:
            |- address: ${b.address}
            |- coords:  ${coordStr(b.coords)}
            |- name:    ${b.name}
            |- outline: 
            |${drawGrid(b.outline, 15)}
            |""".stripMargin
      )
    }
  }
}
