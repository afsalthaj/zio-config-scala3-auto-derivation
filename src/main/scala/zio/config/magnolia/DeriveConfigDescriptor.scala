package zio.config.magnolia

import zio.config._, ConfigDescriptor._
import zio.duration.Duration

import java.io.File
import java.net.{URI, URL}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.concurrent.duration.{Duration => ScalaDuration}

final case class Descriptor[A](desc: ConfigDescriptor[A])

object Descriptor {
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
}