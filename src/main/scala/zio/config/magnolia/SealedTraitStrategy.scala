package zio.config.magnolia

enum SealedTraitSubClassNameStrategy:
  def &&(sealedTraitNameStrategy: SealedTraitNameStrategy): SealedTraitStrategy =
    SealedTraitStrategy(this, sealedTraitNameStrategy)
  
  case WrapSubClassName                    extends SealedTraitSubClassNameStrategy
  case IgnoreSubClassName                  extends SealedTraitSubClassNameStrategy
  case LabelSubClassName(fieldName: String) extends SealedTraitSubClassNameStrategy


enum SealedTraitNameStrategy:
  def &&(subClassNameStrategy: SealedTraitSubClassNameStrategy): SealedTraitStrategy =
    SealedTraitStrategy(subClassNameStrategy, this)
  
  case WrapSealedTraitName   extends SealedTraitNameStrategy
  case IgnoreSealedTraitName extends SealedTraitNameStrategy


final case class SealedTraitStrategy(
  subClass: SealedTraitSubClassNameStrategy,
  parentClass: SealedTraitNameStrategy
)

object SealedTraitStrategy {
  import SealedTraitNameStrategy._
  import SealedTraitSubClassNameStrategy._

  def wrapSealedTraitName: SealedTraitNameStrategy   = WrapSealedTraitName
  def ignoreSealedTraitName: SealedTraitNameStrategy = IgnoreSealedTraitName

  def wrapSubClassName: SealedTraitSubClassNameStrategy                     = WrapSubClassName
  def ignoreSubClassName: SealedTraitSubClassNameStrategy                   = IgnoreSubClassName
  def labelSubClassName(fieldName: String): SealedTraitSubClassNameStrategy = LabelSubClassName(fieldName)
  def pureConfig = labelSubClassName("type")
}