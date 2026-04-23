// Fragment shader: evaluate Stokes stream function at each pixel
//
// ψ = Im[conj(z) f(z) + g(z)]
// where f(z) = Σ c_k/(z-β_k) + Σ a_n z^n   (Goursat function)
//       g(z) = Σ d_k/(z-β_k) + Σ b_n z^n   (Goursat function)

struct VertexOutput {
  @builtin(position) position: vec4<f32>,
  @location(0) uv: vec2<f32>,
};

struct Params {
  viewInverse: mat4x4<f32>,
  nPoles: u32,
  nPolyF: u32,
  nPolyG: u32,
  _pad: u32,
  psiMin: f32,
  psiMax: f32,
  _pad2: f32,
  _pad3: f32,
}

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var<storage, read> poles: array<vec2<f32>>;
@group(0) @binding(2) var<storage, read> fRatCoeffs: array<vec2<f32>>;
@group(0) @binding(3) var<storage, read> fPolyCoeffs: array<vec2<f32>>;
@group(0) @binding(4) var colorscale_texture: texture_2d<f32>;
@group(0) @binding(5) var colorscale_sampler: sampler;
@group(0) @binding(6) var<storage, read> gRatCoeffs: array<vec2<f32>>;
@group(0) @binding(7) var<storage, read> gPolyCoeffs: array<vec2<f32>>;
@group(0) @binding(8) var<storage, read> domainVerts: array<vec2<f32>>;
@group(0) @binding(9) var<uniform> nDomainVerts: u32;

fn cmul(a: vec2<f32>, b: vec2<f32>) -> vec2<f32> {
  return vec2<f32>(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

fn pointInPolygon(p: vec2<f32>, nVerts: u32) -> bool {
  var crossings: u32 = 0u;
  for (var i: u32 = 0u; i < nVerts; i = i + 1u) {
    let v1 = domainVerts[i];
    let v2 = domainVerts[(i + 1u) % nVerts];
    if ((v1.y <= p.y && v2.y > p.y) || (v2.y <= p.y && v1.y > p.y)) {
      let t = (p.y - v1.y) / (v2.y - v1.y);
      if (p.x < v1.x + t * (v2.x - v1.x)) {
        crossings = crossings + 1u;
      }
    }
  }
  return (crossings % 2u) == 1u;
}

// Evaluate rational + polynomial: returns (value, derivative)
fn evalRP(z: vec2<f32>, isG: bool) -> array<vec2<f32>, 2> {
  var val = vec2<f32>(0.0, 0.0);
  var deriv = vec2<f32>(0.0, 0.0);

  // Rational part: Σ c_k / (z - β_k)
  for (var k: u32 = 0u; k < params.nPoles; k = k + 1u) {
    let dz = z - poles[k];
    let d = dot(dz, dz);
    let inv = vec2<f32>(dz.x / d, -dz.y / d);
    // -1/(z-β)² = -(dz²)/d² where dz² = (dx²-dy², 2dxdy)
    let d2 = d * d;
    let inv2 = vec2<f32>(-(dz.x*dz.x - dz.y*dz.y) / d2, 2.0*dz.x*dz.y / d2);

    var ck: vec2<f32>;
    if (isG) { ck = gRatCoeffs[k]; } else { ck = fRatCoeffs[k]; }
    val = val + cmul(ck, inv);
    deriv = deriv + cmul(ck, inv2);
  }

  // Polynomial part: Σ a_n z^n
  let nP = select(params.nPolyF, params.nPolyG, isG);
  var power = vec2<f32>(1.0, 0.0);
  for (var n: u32 = 0u; n < nP; n = n + 1u) {
    var an: vec2<f32>;
    if (isG) { an = gPolyCoeffs[n]; } else { an = fPolyCoeffs[n]; }
    val = val + cmul(an, power);
    if (n >= 1u) {
      var dpow = vec2<f32>(1.0, 0.0);
      for (var j: u32 = 0u; j < n - 1u; j = j + 1u) { dpow = cmul(dpow, z); }
      deriv = deriv + cmul(vec2<f32>(f32(n) * an.x, f32(n) * an.y), dpow);
    }
    power = cmul(power, z);
  }

  return array<vec2<f32>, 2>(val, deriv);
}

@fragment
fn main(input: VertexOutput) -> @location(0) vec4<f32> {
  let worldPos = params.viewInverse * vec4<f32>(input.uv, 0.0, 1.0);
  let z = worldPos.xy;

  let inside = pointInPolygon(z, nDomainVerts);

  // Evaluate Goursat functions
  let fResult = evalRP(z, false);
  let f_val = fResult[0];
  let f_deriv = fResult[1];
  let gResult = evalRP(z, true);
  let g_val = gResult[0];
  let g_deriv = gResult[1];

  // ψ = Im[conj(z) * f(z) + g(z)]
  let zbar = vec2<f32>(z.x, -z.y);
  let zbar_f = cmul(zbar, f_val);
  let psi = zbar_f.y + g_val.y;

  // Velocity: u - iv = -conj(f) + conj(z)*f' + g'
  let neg_fbar = vec2<f32>(-f_val.x, f_val.y);
  let zbar_fp = cmul(zbar, f_deriv);
  let vel = neg_fbar + zbar_fp + g_deriv;
  let speed = length(vel);

  // Visualize stream function with contours (must be in uniform control flow)
  let t = clamp((psi - params.psiMin) / (params.psiMax - params.psiMin), 0.0, 1.0);
  let color = textureSample(colorscale_texture, colorscale_sampler, vec2<f32>(t, 0.5));

  let contourInterval = (params.psiMax - params.psiMin) / 20.0;
  let dpsi_dx = dpdx(psi);
  let dpsi_dy = dpdy(psi);
  let psi_grad = length(vec2<f32>(dpsi_dx, dpsi_dy));
  let psi_dist = abs(fract(psi / contourInterval - 0.5) - 0.5) * contourInterval / psi_grad;
  let contour_alpha = 1.0 - smoothstep(0.5, 1.5, psi_dist);

  if (!inside) {
    return vec4<f32>(1.0, 1.0, 1.0, 1.0);
  }

  let final_color = mix(color.rgb, vec3<f32>(0.0, 0.0, 0.0), contour_alpha * 0.4);
  return vec4<f32>(final_color, 1.0);
}
