package de.hetzge.sgame.mapgen

object Test extends App {
  implicit def stringToCharArray(value: String) = value.toCharArray()
  val TOP = TileDefinition("XXXOOOOOO", "XXOOOOOOO", "OXXOOOOOO", "OXOOOOOOO")
  val copy = TOP.copy(1)
  copy.fields.foreach{ field => 
    field.foreach(print)
    println("")
  }
}

