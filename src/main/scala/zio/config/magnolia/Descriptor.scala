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
import scala.quoted.*

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
          allPossibleParentNames
        )

      case a: Mirror.ProductOf[T] => 
        descriptorOfProduct(
          descriptors, 
          elemLabels, 
          allPossibleParentNames, 
          lst => a.fromProduct(Tuple.fromArray(lst.toArray[Any])),
          t => t.asInstanceOf[Product].productIterator.toList
        )

   def descriptorOfSum[T](
     allDescs: => List[Descriptor[T]],
     possibleNames: => List[String],
     st: SealedTraitNameStrategy = SealedTraitNameStrategy.IgnoreSealedTraitName
   ): Descriptor[T] =  {
     import SealedTraitNameStrategy._ 
     
     val desc = 
      allDescs.reduce((a, b) => Descriptor(a.desc.orElse(b.desc)))

     st match {
       case WrapSealedTraitName =>
        val descAtAllPaths =  
          possibleNames.map(n => nested(n)(desc.desc)).reduce(_ orElse _)

        Descriptor(descAtAllPaths)

       case IgnoreSealedTraitName => 
         desc
     }
    }

   def descriptorOfProduct[T](
     allDescs: => List[Descriptor[_]],
     elemLabels: => List[String],
     possibleNames: => List[String],
     f: List[Any] => T,
     g: T => List[Any]
   ): Descriptor[T] =
      if elemLabels.isEmpty then
          val tryAllPaths = possibleNames.map(n => Constant.mk(n)).reduce(_ orElse _)
          Descriptor(tryAllPaths.transform[T](_ => f(List.empty[Any]), _.toString))
      else
        val listOfDescriptions = 
          elemLabels.zip(allDescs).map({case (a, b) => nested(a)(b.desc.asInstanceOf[ConfigDescriptor[Any]])})
        
        Descriptor(collectAll(listOfDescriptions.head, listOfDescriptions.tail :_*).transform[T](f, g))

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