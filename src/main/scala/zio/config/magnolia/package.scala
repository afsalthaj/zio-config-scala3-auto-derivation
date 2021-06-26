package zio.config

package object magnolia {
  def desriptor[A](implicit ev: Descriptor[A]) =
    ev.desc
}
