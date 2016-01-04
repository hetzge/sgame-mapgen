package de.hetzge.sgame.mapgen

import com.badlogic.gdx.ApplicationListener
import com.badlogic.gdx.ApplicationAdapter
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration
import com.badlogic.gdx.backends.lwjgl.LwjglApplication
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx._
import com.badlogic.gdx.graphics.g2d.TextureRegion
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.OrthographicCamera

object MapGen extends App {
  val config = new LwjglApplicationConfiguration()
  config.width = 1400
  config.height = 800
  new LwjglApplication(Game, config)
}

case class Orientation(xOffset: Int, yOffset: Int)
object Orientation {
  val LEFT = Orientation(-1, 0)
  val RIGHT = Orientation(1, 0)
  val TOP = Orientation(0, -1)
  val BOTTOM = Orientation(0, 1)
  val LEFT_TOP = Orientation(-1, -1)
  val LEFT_BOTTOM = Orientation(-1, 1)
  val RIGHT_TOP = Orientation(1, -1)
  val RIGHT_BOTTOM = Orientation(1, 1)
  val NONE = Orientation(0, 0)

  val SIMPLE = List(LEFT, RIGHT, TOP, BOTTOM)
  val COMPLEX = List(LEFT_TOP, TOP, RIGHT_TOP, RIGHT, RIGHT_BOTTOM, BOTTOM, LEFT_BOTTOM, LEFT)

  /**
   * Important: Don't change the order.
   */
  val FIELD = List(LEFT_TOP, TOP, RIGHT_TOP, LEFT, NONE, RIGHT, LEFT_BOTTOM, BOTTOM, RIGHT_BOTTOM)

  val EDGE_ONE = List(Orientation.LEFT, Orientation.LEFT_TOP, Orientation.TOP)
  val EDGE_TWO = List(Orientation.TOP, Orientation.RIGHT_TOP, Orientation.RIGHT)
  val EDGE_THREE = List(Orientation.RIGHT, Orientation.RIGHT_BOTTOM, Orientation.BOTTOM)
  val EDGE_FOUR = List(Orientation.BOTTOM, Orientation.LEFT_BOTTOM, Orientation.LEFT)

  val EDGES = List(EDGE_ONE, EDGE_TWO, EDGE_THREE, EDGE_FOUR)
}

case class TileConfig(width: Int, height: Int, border: Int)
case class Ground(name: String, walkable: Boolean = true)
case class GroundRule(from: Ground, to: Ground) {
  def contains(ground: Ground): Boolean = List(from, to).contains(ground)
}
case class TileDefinition(fields: Array[Char]*) {

  def copy(rotationTimes: Int): TileDefinition = {
    val rotatedFields = fields.collect { case field => rotate(field, rotationTimes) }
    TileDefinition(rotatedFields: _*)
  }

  def rotate(array: Array[Char], times: Int): Array[Char] = {
    0.until(times).foldLeft(array)((a, b) => rotate(a))
  }

  def rotate(array: Array[Char]): Array[Char] = {
    val SIDE = 3
    val indexer = new Indexer(SIDE, SIDE)
    val result = new Array[Char](array.length)

    for (i <- 0 until SIDE) {
      for (j <- 0 until SIDE) {
        result(indexer.index(i, j)) = array(indexer.index(SIDE - j - 1, i))
      }
    }
    result
  }

}
case class TileSet(groundRule: GroundRule, mapping: Map[TileDefinition, Int]) {

}
object TileDefinition {
  implicit def stringToCharArray(value: String) = value.toCharArray()

  val PRIMARY = 'X'
  val SECONDARY = 'O'

  val FULL = TileDefinition("XXXXXXXXX")
  val EMPTY = TileDefinition("OOOOOOOOO")
  val CENTER = TileDefinition("OOOOXOOOO")

  // TODO zusammenfassen
  val TOP = TileDefinition("XXXOOOOOO", "XXOOOOOOO", "OXXOOOOOO", "OXOOOOOOO")
  val BOTTOM = TOP.copy(2)
  val LEFT = TOP.copy(1)
  val RIGHT = TOP.copy(3)

  // TODO zusammenfassen
  val LEFT_TOP = TileDefinition("XOOOOOOOO")
  val RIGHT_TOP = LEFT_TOP.copy(3)
  val LEFT_BOTTOM = LEFT_TOP.copy(1)
  val RIGHT_BOTTOM = LEFT_TOP.copy(2)

  // TODO zusammenfassen
  val CORNER_BOTTOM_RIGHT = TileDefinition("XXXXOOXOO", "XXOXOOXOO", "XXOXOOOOO", "XXXXOOOOO")
  val CORNER_TOP_RIGHT = CORNER_BOTTOM_RIGHT.copy(2)
  val CORNER_TOP_LEFT = CORNER_BOTTOM_RIGHT.copy(1)
  val CORNER_BOTTOM_LEFT = CORNER_BOTTOM_RIGHT.copy(3)

  val ALL = Array(FULL, EMPTY, CENTER, TOP, BOTTOM, LEFT, RIGHT, LEFT_TOP, RIGHT_TOP, LEFT_BOTTOM, RIGHT_BOTTOM,
    CORNER_TOP_RIGHT, CORNER_BOTTOM_RIGHT, CORNER_TOP_LEFT, CORNER_BOTTOM_LEFT);

}
case class GeneratorConfiguration(val width: Int, val height: Int, val primaryTileSet: TileSet, val tileSets: List[TileSet])

object Game extends ApplicationAdapter {

  implicit def idsToTileSetMap(values: Array[Int]): Map[TileDefinition, Int] = {
    (for (i <- values.indices) yield (TileDefinition.ALL(i), values(i))).toMap
  }

  val tileConfig = TileConfig(16, 16, 1)
  val grass = Ground("grass")
  val lowGrass = Ground("lowGrass")
  val desert = Ground("desert")
  val water = Ground("water", false)

  var textureRegions: List[TextureRegion] = null
  var spriteBatch: SpriteBatch = null
  var tileSets: List[TileSet] = null
  var tiles: Array[Int] = null
  var camera: OrthographicCamera = null

  override def create() = {
    camera = new OrthographicCamera(Gdx.graphics.getWidth(), Gdx.graphics.getHeight())
    camera.setToOrtho(true, Gdx.graphics.getWidth(), Gdx.graphics.getHeight());
    spriteBatch = new SpriteBatch()
    spriteBatch.setProjectionMatrix(camera.combined)
    textureRegions = loadTextureRegions()
    tileSets = loadTileSets()
    tiles = new Generator(GeneratorConfiguration(200, 200, tileSets.toList(0), tileSets)).generate()
  }

  def loadTextureRegions() = {
    val texture = new Texture(Gdx.files.internal("assets/tiles.png"))
    val textureWidth = texture.getWidth()
    val textureHeight = texture.getHeight()

    val horizontal = (tileConfig.border to textureWidth by tileConfig.width + tileConfig.border).toList
    val vertical = (tileConfig.border to textureHeight by tileConfig.height + tileConfig.border).toList

    horizontal.collect {
      case y if y <= textureHeight - tileConfig.height => vertical.collect {
        case x if x <= textureWidth - tileConfig.width => {
          val textureRegion = new TextureRegion(texture, x, y, tileConfig.width, tileConfig.height)
          textureRegion.flip(false, true)
          textureRegion
        }
      }
    }.flatten
  }

  def loadTileSets(): List[TileSet] = {
    val i = new Indexer(11, 11)
    val grassDesertTileSet = TileSet(GroundRule(grass, desert), Map(
      TileDefinition.FULL -> i(1, 1),
      TileDefinition.CENTER -> i(8, 2),
      TileDefinition.EMPTY -> i(5, 2),
      TileDefinition.BOTTOM -> i(1, 0),
      TileDefinition.TOP -> i(1, 2),
      TileDefinition.LEFT -> i(2, 1),
      TileDefinition.RIGHT -> i(0, 1),
      TileDefinition.LEFT_BOTTOM -> i(2, 0),
      TileDefinition.RIGHT_BOTTOM -> i(0, 0),
      TileDefinition.LEFT_TOP -> i(2, 2),
      TileDefinition.RIGHT_TOP -> i(0, 2),
      TileDefinition.CORNER_BOTTOM_LEFT -> i(4, 0),
      TileDefinition.CORNER_BOTTOM_RIGHT -> i(3, 0),
      TileDefinition.CORNER_TOP_LEFT -> i(4, 1),
      TileDefinition.CORNER_TOP_RIGHT -> i(3, 1)))
    val grassWaterTileSet = TileSet(GroundRule(grass, water), Map(
      TileDefinition.FULL -> i(1, 1),
      TileDefinition.CENTER -> i(7, 0),
      TileDefinition.EMPTY -> i(7, 9),
      TileDefinition.BOTTOM -> i(7, 10),
      TileDefinition.TOP -> i(7, 8),
      TileDefinition.LEFT -> i(6, 9),
      TileDefinition.RIGHT -> i(8, 9),
      TileDefinition.LEFT_BOTTOM -> i(10, 7),
      TileDefinition.RIGHT_BOTTOM -> i(9, 7),
      TileDefinition.LEFT_TOP -> i(10, 8),
      TileDefinition.RIGHT_TOP -> i(9, 8),
      TileDefinition.CORNER_BOTTOM_LEFT -> i(8, 8),
      TileDefinition.CORNER_BOTTOM_RIGHT -> i(6, 8),
      TileDefinition.CORNER_TOP_LEFT -> i(8, 10),
      TileDefinition.CORNER_TOP_RIGHT -> i(6, 10)))
    List(grassDesertTileSet, grassWaterTileSet)
  }

  override def render() = {
    spriteBatch.begin()

    val indexer = new Indexer(200, 200)
    for (i <- tiles.indices) {
      val (x, y) = indexer.unIndex(i)
      spriteBatch.draw(textureRegions(tiles(i)), x * 16, y * 16)
    }
    spriteBatch.end()
  }

}

class Indexer(val width: Int, val height: Int) {
  def index(x: Int, y: Int): Int = x + y * width
  def unIndex(index: Int): (Int, Int) = (index % width, Math.floor(index / width).toInt)
  def apply(x: Int, y: Int): Int = index(x, y)

  def around(orientation: Orientation, index: Int): Option[Int] = {
    val (x, y) = unIndex(index)
    val (nextX, nextY) = (x + orientation.xOffset, y + orientation.yOffset)
    if (nextX < 0 || nextY < 0 || nextX >= width || nextY >= height) {
      None
    } else {
      Some(this.index(nextX, nextY))
    }
  }
}

class Generator(val config: GeneratorConfiguration) {
  val width = config.width
  val height = config.height
  val length = width * height
  val ground = new Array[Ground](length)
  val result = new Array[Int](length)
  val indexer = new Indexer(config.width, config.height)

  def generate(): Array[Int] = {
    // default
    for (i <- result.indices) {
      ground(i) = config.primaryTileSet.groundRule.from
      result(i) = config.primaryTileSet.mapping(TileDefinition.FULL)
    }

    // generate grounds
    for (tileSet <- config.tileSets) {

      println("----------")

      for (i <- 0 to 25) {
        val randomX = (Math.random() * width).toInt
        val randomY = (Math.random() * height).toInt
        iterate(randomX, randomY, 25, index => {
          if (checkAround(index, tileSet.groundRule)) {
            ground(index) = tileSet.groundRule.to
            true
          } else {
            false
          }
        }, 0.5d)
      }

      // clean grounds
      for (index <- result.indices) {
        if (ground(index) == tileSet.groundRule.to && !checkAround3Rule(index)) {
          ground(index) = tileSet.groundRule.from
          //result(index) = 10
        }
      }

      // test
      ground(indexer.index(10, 10)) = config.primaryTileSet.groundRule.to
      ground(indexer.index(11, 10)) = config.primaryTileSet.groundRule.to
      ground(indexer.index(12, 10)) = config.primaryTileSet.groundRule.to

      ground(indexer.index(10, 11)) = config.primaryTileSet.groundRule.to
      ground(indexer.index(11, 11)) = config.primaryTileSet.groundRule.to
      ground(indexer.index(12, 11)) = config.primaryTileSet.groundRule.to

      ground(indexer.index(10, 12)) = config.primaryTileSet.groundRule.to
      ground(indexer.index(11, 12)) = config.primaryTileSet.groundRule.to
      ground(indexer.index(12, 12)) = config.primaryTileSet.groundRule.to
      //      val bla = resolveTileDefinition(config.primaryTileSet, indexer.index(11, 11))
      //      bla match {
      //        case Some(tileDefinition) => tileDefinition.fields.foreach(println)
      //        case None => println("leider nichts")
      //      }

      // set result
      for (index <- result.indices) {
        if (tileSet.groundRule.to == ground(index)) {
          val tileDefinitionOption = resolveTileDefinition(tileSet, index)
          tileDefinitionOption match {
            case Some(tileDefinition) => result(index) = tileSet.mapping(tileDefinition)
            case None => result(index) = 111
          }
        }

        // TODO TEMP
        //        if (result(index) != 10) {
        //          if (ground(index) == tileSet.groundRule.from) {
        //            result(index) = tileSet.mapping(TileDefinition.FULL)
        //          } else if (ground(index) == tileSet.groundRule.to) {
        //            result(index) = tileSet.mapping(TileDefinition.EMPTY)
        //          }
        //        }
      }

    }

    return result
  }

  def resolveTileDefinition(tileSet: TileSet, index: Int): Option[TileDefinition] = {
    resolveTileDefinition0(tileSet, index) // .orElse(resolveAlternativeTile(tileSet, index))
  }

  def resolveAlternativeTile(tileSet: TileSet, index: Int): Option[TileDefinition] = {
    val tileDefinitionResults = List(TileDefinition.CORNER_TOP_LEFT, TileDefinition.CORNER_BOTTOM_LEFT, TileDefinition.CORNER_BOTTOM_RIGHT, TileDefinition.CORNER_TOP_RIGHT)
    val zipped = Orientation.EDGES.zip(tileDefinitionResults)
    val find = zipped.find { case (orientations, tileDefinition) => alternativeCheck(orientations, tileSet, index) }
    find match {
      case Some((a, b)) => Some(b)
      case None => None
    }
  }

  def alternativeCheck(orientations: List[Orientation], tileSet: TileSet, index: Int): Boolean = {
    orientations.forall { orientation => indexer.around(orientation, index).exists { nextIndex => ground(nextIndex) == tileSet.groundRule.to } }
  }

  def resolveTileDefinition0(tileSet: TileSet, index: Int, level: Int = 0): Option[TileDefinition] = {
    if (level > 4) {
      return None
    }
    tileSet.mapping.keys.find {
      case tileDefinition => {
        Orientation.FIELD.indices.forall { i =>
          val orientation = Orientation.FIELD(i)
          val fieldsLength = tileDefinition.fields.length
          val normalizedLevel = if (level >= fieldsLength) fieldsLength - 1 else level
          val field = tileDefinition.fields(normalizedLevel)(i)
          val nextIndexOption = indexer.around(orientation, index)
          nextIndexOption match {
            case Some(nextIndex) => (field == TileDefinition.PRIMARY && ground(nextIndex) == tileSet.groundRule.from) || (field == TileDefinition.SECONDARY && ground(nextIndex) == tileSet.groundRule.to)
            case None => true
          }
        }
      }
    }.orElse(resolveTileDefinition0(tileSet, index, level + 1))
  }

  //  TileDefinition.PRIMARY.toLower == field

  def checkAround(index: Int, groundRule: GroundRule): Boolean = {
    val (x, y) = indexer.unIndex(index)
    Orientation.COMPLEX.forall { orientation =>
      val nextX = x + orientation.xOffset
      val nextY = y + orientation.yOffset
      if (nextX >= 0 && nextX < width && nextY >= 0 && nextY < height) {
        val nextIndex = indexer.index(nextX, nextY)
        ground(nextIndex) == groundRule.from || ground(nextIndex) == groundRule.to
      } else {
        true
      }
    }
  }

  def checkAround3Rule(index: Int): Boolean = {
    val (x, y) = indexer.unIndex(index)
    val currentGround = ground(index)
    Orientation.EDGES.exists { orientations =>
      orientations.forall { orientation =>
        val nextX = x + orientation.xOffset
        val nextY = y + orientation.yOffset
        if (nextX >= 0 && nextX < width && nextY >= 0 && nextY < height) {
          val nextIndex = indexer.index(nextX, nextY)
          val nextGround = ground(nextIndex)
          nextGround == currentGround
        } else {
          true
        }
      }
    }
  }

  def iterate(x: Int, y: Int, depth: Int, function: (Int) => Boolean, random: Double = 1d, blacklist: scala.collection.mutable.Set[Int] = scala.collection.mutable.Set(), counter: Int = 0): Unit = {
    val index = indexer.index(x, y)
    if (!blacklist.contains(index) && x >= 0 && x < width && y >= 0 && y < height) {
      blacklist.add(index)
      if (function(index)) {
        if (counter <= depth) {
          val nextCounter = counter + 1
          Orientation.SIMPLE.foreach { orientation =>
            if (Math.random() <= random) {
              val nextX = x + orientation.xOffset
              val nextY = y + orientation.yOffset
              iterate(nextX, nextY, depth, function, random, blacklist, nextCounter)
            }
          }
        }
      }
    }
  }

}


