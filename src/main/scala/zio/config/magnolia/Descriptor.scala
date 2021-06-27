package zio.config.magnolia

import zio.config._, ConfigDescriptor._
import zio.duration.Duration

import java.io.File
import java.net.{URI, URL}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.concurrent.duration.{Duration => ScalaDuration}
import ConfigDescriptorAdt._
import scala.deriving._
import scala.compiletime.{erasedValue, summonInline, constValue, error, codeOf}

final case class Descriptor[A](desc: ConfigDescriptor[A])

object Descriptor {
  final case class Field[T](path: String)

  def apply[A](implicit ev: Descriptor[A]) = 
    ev.desc
    
  given Descriptor[String] = Descriptor(string)
  given Descriptor[Boolean] = Descriptor(boolean)
  given Descriptor[Byte] = Descriptor(byte)
  given Descriptor[Short] = Descriptor(short)
  given Descriptor[Int] = Descriptor(int)
  given Descriptor[Long] = Descriptor(long)
  given Descriptor[BigInt] = Descriptor(bigInt)
  given Descriptor[Float] = Descriptor(float)
  given Descriptor[Double] = Descriptor(double)
  given Descriptor[BigDecimal] = Descriptor(bigDecimal)
  given Descriptor[URI] = Descriptor(uri)
  given Descriptor[URL] = Descriptor(url)
  given Descriptor[ScalaDuration] = Descriptor(duration)
  given Descriptor[Duration] = Descriptor(zioDuration)
  given Descriptor[UUID] = Descriptor(uuid)
  given Descriptor[LocalDate] = Descriptor(localDate)
  given Descriptor[LocalTime] = Descriptor(localTime)
  given Descriptor[LocalDateTime] = Descriptor(localDateTime)
  given Descriptor[Instant] = Descriptor(instant)
  given Descriptor[File] = Descriptor(file)
  given Descriptor[java.nio.file.Path] = Descriptor(javaFilePath)

  given eitherDesc[A, B](using ev1: Descriptor[A], ev2: Descriptor[B]): Descriptor[Either[A, B]] = 
    Descriptor(ev1.desc.orElseEither(ev2.desc))

  given optDesc[A](using ev: Descriptor[A]): Descriptor[Option[A]] = 
    Descriptor(ev.desc.optional)

  given listDesc[A](using ev: Descriptor[A]): Descriptor[List[A]] = 
    Descriptor(list(ev.desc))

  given mapDesc[A](using ev: Descriptor[A]): Descriptor[Map[String, A]] = 
    Descriptor(map(ev.desc))  

  inline given derived[T](using m: Mirror.Of[T]): Descriptor[T] =
    lazy val descriptors = summonDescriptorAll[m.MirroredElemTypes]
    lazy val label = constValue[m.MirroredLabel]
    lazy val elemLabels = labelsToList[m.MirroredElemLabels]

    inline m match
      case s: Mirror.SumOf[T] => 
        descriptorOfSum(descriptors.map(_.asInstanceOf[Descriptor[T]]))
      case a: Mirror.ProductOf[T] => 
        descriptorOfProduct(
          descriptors, 
          elemLabels, 
          label, 
          lst => a.fromProduct(Tuple.fromArray(lst.toArray[Any])),
          t => t.asInstanceOf[Product].productIterator.toList
        )

   def descriptorOfSum[T](
     allDescs: => List[Descriptor[T]]
   ): Descriptor[T] = 
    allDescs.reduce((a, b) => Descriptor(a.desc.orElse(b.desc)))

   def descriptorOfProduct[T](
     allDescs: => List[Descriptor[_]],
     elemLabels: => List[String],
     label: => String,
     f: List[Any] => T,
     g: T => List[Any]
   ): Descriptor[T] =
      if elemLabels.isEmpty then
          Descriptor(Constant.mk(label).transform[T](_ => f(List.empty[Any]), _.toString))
      else
        val listOfDescriptions = 
          elemLabels.zip(allDescs).map({case (a, b) => nested(a)(b.desc.asInstanceOf[ConfigDescriptor[Any]])})
        
        Descriptor(collectAll(listOfDescriptions.head, listOfDescriptions.tail :_*).transform[T](f, g))

  inline def summonDescriptorAll[T <: Tuple]: List[Descriptor[_]] = 
    inline erasedValue[T] match
      case _ : EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Descriptor[t]] :: summonDescriptorAll[ts]

  inline def labelsToList[T <: Tuple]: List[String] = 
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _ : ( t *: ts) => constValue[t].toString :: labelsToList[ts]
}