package zio.config.magnolia

import zio.config.magnolia._
import zio.config._, ConfigDescriptor._, ConfigDescriptorAdt._

final case class A(x: B)
final case class B(y: String, z: C, l: List[C], f: Option[C], g: Either[C, E])
final case class C()

sealed trait E
case object D extends E
case object F extends E

object Main:
  def main(args: Array[String]): Unit =
    val source = ConfigSource.fromMap(Map("x.y" -> "hi", "x.z" -> "C", "x.l" -> "C, C", "x.g" -> "F"), keyDelimiter = Some('.'), valueDelimiter = Some(','))
   
    val res = read(descriptor[A] from source)

    println(res)
    // Right(A(B(hi,C(),List(C(), C()),None,Right(D))))

end Main
