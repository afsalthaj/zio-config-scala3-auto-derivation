import zio.config._, ConfigDescriptor._

final case class Hello(value: String)

object Main:
  def main(args: Array[String]): Unit =
    val source = ConfigSource.fromMap(Map("x" -> "v"))
    val res = read(string("x").to[Hello] from source)
    println(res)

end Main
