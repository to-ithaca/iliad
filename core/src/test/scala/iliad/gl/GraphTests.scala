package iliad
package gl

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.Arbitrary._

import shapeless._

class GraphTests extends FunSuite with Matchers with GraphModelArbitraries
    with GeneratorDrivenPropertyChecks {

  test("graph is invalid if a leaf node is offscreen") {
    forAll(arbitrary[GraphModel.Draw.Constructor]) { (n) =>
      val graph = GraphConstruction.put(n).run(GraphModel.Graph.emptyConstructor).value._1
      val validated = GraphConstruction.validate(GraphModel.Graph.Constructed.fromConstructor(graph))
      n.framebuffer match {
        case GraphModel.Framebuffer.OnScreen => validated.isValid shouldBe true
        case _ => validated.isValid shouldBe false
      }
    }
  }
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

  def offScreenFramebufferArbitrary:
      Arbitrary[GraphModel.Framebuffer.OffScreenConstructor] =
    Arbitrary(arbitrary[Map[FramebufferAttachment, GraphModel.Output.Constructor]].map(os =>
      GraphModel.Framebuffer.OffScreenConstructor(os.toList)))

  implicit val framebufferArbitrary: Arbitrary[GraphModel.Framebuffer.Constructor] =
    Arbitrary(
      Gen.oneOf(offScreenFramebufferArbitrary.arbitrary,
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
}
