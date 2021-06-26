package zio.config.magnolia

import zio.config.magnolia._
import zio.config._, ConfigDescriptor._

final case class Hello(value: String)

object Main:
  def main(args: Array[String]): Unit =
    val source = ConfigSource.fromMap(Map("x" -> "v"))
    val res = read((nested("x")(desriptor[String])).to[Hello] from source)
    println(res)

end Main
