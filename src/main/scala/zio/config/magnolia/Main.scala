package zio.config.magnolia

import zio.config.magnolia._
import zio.config._, ConfigDescriptor._, ConfigDescriptorAdt._

final case class A(x: B)
final case class B(y: String, z: C)
final case class C()

object Main:
  def main(args: Array[String]): Unit =
    val source = ConfigSource.fromMap(Map("x.y" -> "hi", "x.z" -> "C"), keyDelimiter = Some('.'))
   
    val res = read(descriptor[A] from source)

    println(res)

end Main
