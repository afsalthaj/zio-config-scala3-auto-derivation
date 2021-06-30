package zio.config.magnolia

import zio.config._, ConfigDescriptor._
import zio.duration.Duration

import java.io.File
import java.net.{URI, URL}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.concurrent.duration.{Duration => ScalaDuration}
import scala.deriving._
import scala.compiletime.{erasedValue, summonInline, constValue}
import scala.quoted

import javax.management.Descriptor

final case class Descriptor[A](desc: ConfigDescriptor[A])

object Descriptor {
  def apply[A](implicit ev: Descriptor[A]) =
    ev.desc

  lazy given Descriptor[String] = Descriptor(string)
  lazy given Descriptor[Boolean] = Descriptor(boolean)
  lazy given Descriptor[Byte] = Descriptor(byte)
  lazy given Descriptor[Short] = Descriptor(short)
  lazy given Descriptor[Int] = Descriptor(int)
  lazy given Descriptor[Long] = Descriptor(long)
  lazy given Descriptor[BigInt] = Descriptor(bigInt)
  lazy given Descriptor[Float] = Descriptor(float)
  lazy given Descriptor[Double] = Descriptor(double)
  lazy given Descriptor[BigDecimal] = Descriptor(bigDecimal)
  lazy given Descriptor[URI] = Descriptor(uri)
  lazy given Descriptor[URL] = Descriptor(url)
  lazy given Descriptor[ScalaDuration] = Descriptor(duration)
  lazy given Descriptor[Duration] = Descriptor(zioDuration)
  lazy given Descriptor[UUID] = Descriptor(uuid)
  lazy given Descriptor[LocalDate] = Descriptor(localDate)
  lazy given Descriptor[LocalTime] = Descriptor(localTime)
  lazy given Descriptor[LocalDateTime] = Descriptor(localDateTime)
  lazy given Descriptor[Instant] = Descriptor(instant)
  lazy given Descriptor[File] = Descriptor(file)
  lazy given Descriptor[java.nio.file.Path] = Descriptor(javaFilePath)

  given eitherDesc[A, B](using ev1: Descriptor[A], ev2: Descriptor[B]): Descriptor[Either[A, B]] =
    Descriptor(ev1.desc.orElseEither(ev2.desc))

  given optDesc[A: Descriptor]: Descriptor[Option[A]] =
    Descriptor(Descriptor[A].optional)

  given listDesc[A](using ev: Descriptor[A]): Descriptor[List[A]] =
    Descriptor(list(ev.desc))

  given mapDesc[A](using ev: Descriptor[A]): Descriptor[Map[String, A]] =
    Descriptor(map(ev.desc))

  inline given derived[T](using m: Mirror.Of[T]): Descriptor[T] =
    lazy val descriptors = summonDescriptorAll[m.MirroredElemTypes]
    lazy val allPossibleFieldNames = summonAllNames[m.MirroredElemTypes]
    lazy val elemLabels = labelsToList[m.MirroredElemLabels]
    lazy val label = constValue[m.MirroredLabel]
    lazy val name = Macros.nameAnnotations[T]
    lazy val names = Macros.namesAnnotations[T]
    lazy val allPossibleParentNames = label :: name.map(_.name) ++ names.flatMap(_.names.toList)

     inline m match
      case s: Mirror.SumOf[T] =>
        descriptorOfSum(
          descriptors.map(_.asInstanceOf[Descriptor[T]]),
          name.map(_.name) ++ names.flatMap(_.names.toList)
        )

      case a: Mirror.ProductOf[T] =>
        descriptorOfProduct(
          descriptors,
          elemLabels,
          List(label),
          name.map(_.name) ++ names.flatMap(_.names.toList),
          lst => a.fromProduct(Tuple.fromArray(lst.toArray[Any])),
          t => t.asInstanceOf[Product].productIterator.toList
        )

   def descriptorOfSum[T](
     allDescs: => List[Descriptor[T]],
     extraNames: => List[String]
   ): Descriptor[T] =
     val desc =
      allDescs.reduce((a, b) => Descriptor(a.desc.orElse(b.desc)))

     if (extraNames.nonEmpty) then
       Descriptor(extraNames.map(n => nested(n)(desc.desc)).reduce(_ orElse _))
     else desc

   def descriptorOfProduct[T](
     allDescs: => List[Descriptor[_]],
     elemLabels: => List[String],
     parentLabels: => List[String],
     extraNames: => List[String],
     f: List[Any] => T,
     g: T => List[Any],
   ): Descriptor[T] =
       if elemLabels.isEmpty then
         val tryAllPaths = parentLabels.map(n => Constant.mk(n)).reduce(_ orElse _)
         Descriptor(tryAllPaths.transform[T](_ => f(List.empty[Any]), _.toString))
       else
         val listOfDescriptions =
           elemLabels.zip(allDescs).map({ case (a, b) => nested(a)(b.desc.asInstanceOf[ConfigDescriptor[Any]]) })

         val desc = collectAll(listOfDescriptions.head, listOfDescriptions.tail: _*).transform[T](f, g)

         if (extraNames.nonEmpty) then
           Descriptor(extraNames.map(n => nested(n)(desc)).reduce(_ orElse _))
         else Descriptor(desc)


  inline def summonAllNames[T <: Tuple]: List[(String, List[Any])] =
    inline erasedValue[T] match
      case _ : EmptyTuple => Nil
      case _: (t *: ts) => Macros.fieldAnnotations[t] ++ summonAllNames[ts]

  inline def summonDescriptorAll[T <: Tuple]: List[Descriptor[_]] =
    inline erasedValue[T] match
      case _ : EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Descriptor[t]] :: summonDescriptorAll[ts]

  inline def labelsToList[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _ : ( t *: ts) => constValue[t].toString :: labelsToList[ts]
}