package iliad
package gfx

case class ShaderFunction(n: String, f: String)

trait ShaderFunctions {

  /** Converts a vector orientation to a rotation matrix
    *
    * #o - axis angle orientation
    *     o.w is the angle in radians
    *     o.xyz is the normalized axis
    */
  val axisAngleMatrix =
    ShaderFunction("axisAngleMatrix", s"""mat4 axisAngleMatrix(vec4 o){
             float c0 = cos(o.w);
             float c1 = 1.0 - c0;
             float c2 = sin(o.w);
             vec3 a = o.xyz;
             return mat4(
                c0 + a.x*a.x*c1    , a.x*a.y*c1 + a.z*c2, a.x*a.z*c1 - a.y*c2, 0.0,
                a.x*a.y*c1 - a.z*c2, c0 + a.y*a.y*c1    , a.z*a.y*c1 + a.x*c2, 0.0,
                a.x*a.z*c1 + a.y*c2, a.y*a.z*c1 - a.x*c2, c0 + a.z*a.z*c1    , 0.0,
                0.0                , 0.0                , 0.0                , 1.0
             );
          }""")

  val quaternionMatrix =
    ShaderFunction("quaternionMatrix", s"""mat4 quaternionMatrix(vec4 q) {
      vec3 v = q.xyz;
        float x2 = 2.0 * v.x * v.x;
        float y2 = 2.0 * v.y * v.y;
        float z2 = 2.0 * v.z * v.z;
        float xy = 2.0 * v.x * v.y;
        float xz = 2.0 * v.x * v.z;
        float yz = 2.0 * v.y * v.z;
        float sx = 2.0 * q.w * v.x;
        float sy = 2.0 * q.w * v.y;
        float sz = 2.0 * q.w * v.z;
        return mat4(
          1.0 - y2 - z2, xy + sz      , xz - sy      , 0.0,
          xy - sz      , 1.0 - x2 - z2, yz + sx      , 0.0,
          xz + sy      , yz - sx      , 1.0 - x2 - y2, 0.0,
          0.0          , 0.0          , 0.0          , 1.0);
      }""")

  /** Rotates a vector by an axis angle orientation
    *
    * #o - the axis angle orientation
    * #x - the vector to rotate
    */
  val rotate = ShaderFunction("rotate", s"""vec3 rotate(vec4 o, vec3 x) {
               mat4 rotation = ${axisAngleMatrix.n}(o);
               vec4 target = vec4(x, 1.0);
               return (rotation * target).xyz;
           }""")

  /** Projects a position into screen space */
  val cameraProjectionSh = ShaderFunction(
      "cameraProjection",
      s"""vec4 cameraProjection(mat4 camera, vec3 p) {
       vec4 wp = camera * vec4(p, 1.0);
       wp.xyz /= wp.w;
       wp.w = 1.0;
       return wp;
      }""")

  /** colour functions */

  /** converts an rgb colour to a hsl colour */
  val toHsl = ShaderFunction(
    "toHsl",
    s"""vec3 toHsl(vec3 c) {
       float m = min(c.r, min(c.g, c.b));
       float M = max(c.r, max(c.g, c.b));
       float chroma = M - m;
       float hue0 = float(chroma != 0.0) *
                    (float(M == c.r) * mod((c.g - c.b)/chroma, 6.0) +
                    float(M == c.g) * ((c.b - c.r)/chroma + 2.0) +
                    float(M == c.b) * ((c.r - c.g)/chroma + 4.0));
       float hue = mod(mod(hue0 / 6.0, 1.0) + 1.0, 1.0);
       float lightness = (M + m) / 2.0;
       float saturation = float(chroma != 0.0) * (chroma /  (1.0 - abs(2.0 * lightness - 1.0))); 
       return vec3(hue, saturation, lightness);
    }""")

  val shiftHue = ShaderFunction(
    "shiftHue",
    s"""float shiftHue(float prev, float offset) {
        return mod(mod(prev + offset, 1.0) + 1.0, 1.0);
    }""")

  val shiftSaturation = ShaderFunction(
    "shiftSaturation",
    s"""float shiftSaturation(float s, float fraction) {
        return s * fraction;
    }""")

  val toRgb = ShaderFunction(
    "toRgb",
    s"""vec3 toRgb(vec3 hsl) {
        float h = hsl.x;
        float s = hsl.y;
        float l = hsl.z;
        float chroma = s * (1.0 - abs(2.0 * l - 1.0));
        float h0 = h * 6.0;
        float x = chroma * (1.0 - abs(mod(h0, 2.0) - 1.0));

        vec3 rgb0 = 
        float(h0 >= 0.0 && h0 <= 1.0) * vec3(chroma, x, 0.0) +
        float(h0 > 1.0 && h0 <= 2.0) * vec3(x, chroma, 0.0) +
        float(h0 > 2.0 && h0 <= 3.0) * vec3(0.0, chroma, x) +
        float(h0 > 3.0 && h0 <= 4.0) * vec3(0.0, x, chroma) +
        float(h0 > 4.0 && h0 <= 5.0) * vec3(x, 0.0, chroma) +
        float(h0 > 5.0 && h0 <= 6.0) * vec3(chroma, 0.0, x);

        float m = l - 0.5 * chroma;
        return rgb0 + m;
}""")
}
