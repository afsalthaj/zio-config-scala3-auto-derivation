package zio.config.magnolia

import zio.config.magnolia._
import zio.config._, ConfigDescriptor._

final case class A(a: B)
final case class B(b: String, c: C, d: List[C], e: Option[C], f: Either[C, E])
final case class C()

sealed trait E
case object D extends E
case object F extends E

object Example extends App:
  val source = 
    ConfigSource.fromMap(
      Map("a.b" -> "hi", "a.c" -> "C", "a.d" -> "C, C", "a.f" -> "F"), 
      keyDelimiter = Some('.'), 
      valueDelimiter = Some(',')
    )
   
  val res = read(descriptor[A] from source)

  println(res)
  // Right(A(B(hi,C(),List(C(), C()),None,Right(F))))

end Example