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

case class ConstantString(value: String) extends PropertyType[String, String] {
    def read(propertyValue: String): Either[PropertyType.PropertyReadError[String], String] =
      if (propertyValue == value) Right(value)
      else Left(PropertyType.PropertyReadError(propertyValue, s"constant string '$value'"))
    def write(a: String): String                                                            = a
  }

  def constantString(value: String): ConfigDescriptor[String] =
    ConfigDescriptorAdt.Source(ConfigSource.empty, ConstantString(value)) ?? s"constant string '$value'"

  def constant[T](label: String, value: T): ConfigDescriptor[T] =
    constantString(label)(_ => value, p => Some(p).filter(_ == value).map(_ => label))

  inline given descriptorOfProduct[T <: Product](using m: Mirror.ProductOf[T]): Descriptor[T] =  {
    val label = constValue[m.MirroredLabel]
    val elemLabels = labelsToList[m.MirroredElemLabels]
    val allDescs = summonDescriptorAll[m.MirroredElemTypes]
  
      if (elemLabels.isEmpty) {
          Descriptor(constantString(label).transform[T](_ => m.fromProduct(Tuple.fromArray(Array.empty[Any])), _ => ???))
      } else {
        val listOfDescriptions = 
          elemLabels.zip(allDescs).map({case (a, b) => nested(a)(b.desc.asInstanceOf[ConfigDescriptor[Any]])})
        
        Descriptor(
          collectAll(listOfDescriptions.head, listOfDescriptions.tail :_*).transform[T](
            lst => m.fromProduct(Tuple.fromArray(lst.toArray[Any])), 
            b => Tuple.fromProductTyped(b).toList
          ))
      }
  }

  inline def summonDescriptorAll[T <: Tuple]: List[Descriptor[_]] = 
    inline erasedValue[T] match {
      case _ : EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Descriptor[t]] :: summonDescriptorAll[ts]
    }

  inline def labelsToList[T <: Tuple]: List[String] = 
    inline erasedValue[T] match {
     case _: EmptyTuple => Nil
      case _ : ( t *: ts) => constValue[t].toString :: labelsToList[ts]
    }
}