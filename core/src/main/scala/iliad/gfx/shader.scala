package iliad
package gfx

case class ShaderFunction(n: String, f: String)

trait ShaderFunctions {

  /** Projects a position into screen space */
  val cameraProjectionSh = ShaderFunction("cameraProjection",
    s"""vec4 cameraProjection(mat4 camera, vec3 p) {
       vec4 wp = camera * vec4(p, 1.0);
       wp.xyz /= wp.w;
       wp.w = 1.0;
       return wp;
      }""")
}
