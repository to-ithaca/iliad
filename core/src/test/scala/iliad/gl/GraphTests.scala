package iliad
package gl

import iliad.gfx._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.Arbitrary._

import shapeless._

import cats._
import cats.implicits._

import CatsExtra._
import Construct._
//import gen._
/*
class GraphTests extends FunSuite with Matchers with GraphModelArbitraries
    with GeneratorDrivenPropertyChecks {
/*
  test("graph is invalid if a leaf node is offscreen") {
    forAll(arbitrary[GraphModel.Draw.Constructor].filterNot(onScreen)) { (n) =>
      val validated = construct(put(n))
      validated.isValid shouldBe false
    }
  }

  test("graph is valid if a leaf node is onscreen") {
    forAll(arbitrary[GraphModel.Draw.Constructor].filter(onScreen)) { (n) =>
      val validated = construct(put(n))
      validated.isValid shouldBe true
    }
  }

  test("graph is valid if nodes have unique names") {
    forAll(validDrawConstructorsArbitrary.arbitrary) { (ns) =>
      val validated = construct(ns.traverseUnit(n => put(n)))
      validated.isValid shouldBe true
    }
  }*/
}

object gen extends GenInstances

trait GenInstances {
  implicit def toGenOps[A](g: Gen[A]): GenOps[A] =
    new GenOps(g)
}

final class GenOps[A](g: Gen[A]) {
  def filterNot(f: A => Boolean): Gen[A] =
    g.filter(a => !f(a))
}

trait GLArbitraries {

  def oneOf[A](set: Set[A]): Arbitrary[A] =
    Arbitrary(Gen.oneOf(set.toSeq))

  implicit val vertexAttribTypeArbitrary: Arbitrary[VertexAttribType] =
    oneOf(SealedEnum.values[VertexAttribType])

  implicit val attributeConstructorArbitrary: Arbitrary[Attribute.Constructor] = Arbitrary {
    for {
      name <- arbitrary[String]
      byteSize <- arbitrary[Int]
      elementSize <- arbitrary[Int]
      aType <- arbitrary[VertexAttribType]
    } yield Attribute.Constructor(name, byteSize, elementSize, aType)
  }

  implicit val textureWrapArbitrary: Arbitrary[TextureWrap] =
    oneOf(SealedEnum.values[TextureWrap])

  implicit val textureMinFilterArbitrary: Arbitrary[TextureMinFilter] =
    oneOf(SealedEnum.values[TextureMinFilter])

  implicit val textureMagFilterArbitrary: Arbitrary[TextureMagFilter] =
    oneOf(SealedEnum.values[TextureMagFilter])

  implicit val samplerConstructorArbitrary: Arbitrary[Sampler.Constructor] = Arbitrary {
    for {
      s <- arbitrary[TextureWrap]
      t <- arbitrary[TextureWrap]
      min <- arbitrary[TextureMinFilter]
      max <- arbitrary[TextureMagFilter]
    } yield Sampler.Constructor(s, t, min, max)
  }

  implicit val vertexShaderArbitrary: Arbitrary[VertexShader.Source] = Arbitrary {
    for {
      text <- arbitrary[String]
      a <- arbitrary[List[Attribute.Constructor]]
      ts <- arbitrary[Map[String, Sampler.Constructor]]
    } yield VertexShader.Source(text, a, ts.toList)
  }

  implicit val fragmentShaderArbitrary: Arbitrary[FragmentShader.Source] = Arbitrary {
    for {
      t <- arbitrary[String]
      ts <- arbitrary[Map[String, Sampler.Constructor]]
    } yield FragmentShader.Source(t, ts.toList)
  }

  implicit val programArbitrary: Arbitrary[Program.Unlinked] = Arbitrary {
    for {
      v <- arbitrary[VertexShader.Source]
      f <- arbitrary[FragmentShader.Source]
    } yield Program.Unlinked(v, f)
  }

  implicit val primitiveArbitrary: Arbitrary[PrimitiveType] =
    oneOf(SealedEnum.values[PrimitiveType])

  implicit val capabilityArbitrary: Arbitrary[Capability] =
    oneOf(SealedEnum.values[Capability])

  implicit val colorMaskArbitrary: Arbitrary[ColorMask] = Arbitrary {
    for {
      r <- arbitrary[Boolean]
      g <- arbitrary[Boolean]
      b <- arbitrary[Boolean]
      a <- arbitrary[Boolean]
    } yield ColorMask(r, g, b, a)
  }

  implicit val renderbufferArbitrary: Arbitrary[RenderbufferInternalFormat] =
    oneOf(SealedEnum.values[RenderbufferInternalFormat])

  implicit val framebufferAttachmentArbitrary: Arbitrary[FramebufferAttachment] =
    oneOf(SealedEnum.values[FramebufferAttachment])
    
  implicit val texturePixelFormatArbitrary: Arbitrary[TextureFormat] =
    oneOf(SealedEnum.values[TextureFormat])

  //FIXME: there is a bug with the SealedEnum macro - TextureInternalFormat cannot be expanded
  implicit val textureInternalFormatArbitrary: Arbitrary[TextureInternalFormat] =
    oneOf(Set(GL_RGB, GL_RGBA))

  implicit val texturePixelTypeArbitrary: Arbitrary[TexturePixelType] =
    oneOf(SealedEnum.values[TexturePixelType])

  implicit val textureFormatArbitrary: Arbitrary[Texture.Format] = Arbitrary {
    for {
      pixel <- arbitrary[TextureFormat]
      internal <- arbitrary[TextureInternalFormat]
      pixelType <- arbitrary[TexturePixelType]
      bytesPerPixel <- arbitrary[Int]
    } yield Texture.Format(pixel, internal, pixelType, bytesPerPixel)
  }
}

trait GraphModelArbitraries extends GLArbitraries with iliad.ArbitraryInstances {

  implicit val textureConstructorArbitrary: Arbitrary[GraphModel.Texture.Constructor] =
    Arbitrary {
      for {
        n <- arbitrary[String]
      f <- arbitrary[Texture.Format]
      v <- vectorDArbitrary[Nat._2, Int].arbitrary
      d <- arbitrary[Boolean]
    } yield GraphModel.Texture.Constructor(n, f, v, d)
  }

  implicit val renderbufferConstructorArbitrary: Arbitrary[GraphModel.Renderbuffer.Constructor] =
    Arbitrary {
      for {
        n <- arbitrary[String]
        f <- arbitrary[RenderbufferInternalFormat]
        v <- vectorDArbitrary[Nat._2, Int].arbitrary
      } yield GraphModel.Renderbuffer.Constructor(n, f, v)
    }

  implicit val modelConstructorArbitrary: Arbitrary[GraphModel.Model.Constructor] =
    Arbitrary(arbitrary[String].map(GraphModel.Model.Constructor))

  implicit val outputConstructorArbitrary: Arbitrary[GraphModel.Output.Constructor] =
    Arbitrary(Gen.oneOf(
        arbitrary[GraphModel.Renderbuffer.Constructor],
        arbitrary[GraphModel.Texture.Constructor]))

  implicit val framebufferArbitrary: Arbitrary[GraphModel.Framebuffer.Constructor] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[Map[FramebufferAttachment, GraphModel.Output.Constructor]].map(os =>
          GraphModel.Framebuffer.OffScreenConstructor(os.toList)),
      Gen.oneOf(Seq(GraphModel.Framebuffer.OnScreen))))

  implicit val drawConstructorArbitrary:
      Arbitrary[GraphModel.Draw.Constructor] = Arbitrary {
    for {
      n <- arbitrary[String]
      prg <- arbitrary[Program.Unlinked]
      prm <- arbitrary[PrimitiveType]
      cs <- arbitrary[Set[Capability]]
      cm <- arbitrary[ColorMask]
      inst <- arbitrary[Boolean]
      m <- arbitrary[GraphModel.Model.Constructor]
      f <- arbitrary[GraphModel.Framebuffer.Constructor]
    } yield GraphModel.Draw.Constructor(n, prg, prm, cs, cm, inst, m, f)
  }

  val validDrawConstructorsArbitrary:
      Arbitrary[List[GraphModel.Draw.Constructor]] = Arbitrary {
    for {
      count <- Gen.choose(1, 3)
      ns <- Gen.buildableOfN[Set[String],String](count,arbitrary[String]).map(_.toList)
      prgs <- Gen.listOfN(count, arbitrary[Program.Unlinked])
      prms <- Gen.listOfN(count, arbitrary[PrimitiveType])
      css <- Gen.listOfN(count, arbitrary[Set[Capability]])
      cms <- Gen.listOfN(count, arbitrary[ColorMask])
      insts <- Gen.listOfN(count, arbitrary[Boolean])
      ms <- Gen.listOfN(count, arbitrary[GraphModel.Model.Constructor])
      fs <- Gen.listOfN(count, arbitrary[GraphModel.Framebuffer.Constructor].filter({
        case GraphModel.Framebuffer.OnScreen => true
        case _ => false
      }))
    } yield (ns zip prgs zip prms zip css zip cms zip insts zip ms zip fs).map {
      case (((((((n, prg), prm), cs), cm), inst), m), f) =>
        GraphModel.Draw.Constructor(n, prg, prm, cs, cm, inst, m, f)
    }
  }

  def onScreen(n: GraphModel.Draw.Constructor): Boolean = n.framebuffer match {
    case GraphModel.Framebuffer.OnScreen => true
    case _ => false
  }
}
 */
