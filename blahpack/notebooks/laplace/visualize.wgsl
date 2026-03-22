// Fragment shader: evaluate Laplace solution at each pixel
//
// The solution is w(z) = smooth(z) + singular(z) + imCorr*i
// where smooth(z) = Σ a_n (z-c)^n
//       singular(z) = Σ b_k / (z - p_k) + b0

struct VertexOutput {
  @builtin(position) position: vec4<f32>,
  @location(0) uv: vec2<f32>,
};

struct Params {
  viewInverse: mat4x4<f32>,
  nSmooth: u32,
  nPoles: u32,
  center: vec2<f32>,
  b0: vec2<f32>,
  imCorr: f32,
  valueMin: f32,
  valueMax: f32,
  _pad: f32,
}

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var<storage, read> smoothCoeffs: array<vec2<f32>>;
@group(0) @binding(2) var<storage, read> poles: array<vec2<f32>>;
@group(0) @binding(3) var<storage, read> singularCoeffs: array<vec2<f32>>;
@group(0) @binding(4) var colorscale_texture: texture_2d<f32>;
@group(0) @binding(5) var colorscale_sampler: sampler;
@group(0) @binding(6) var<storage, read> vertices: array<vec2<f32>>;
@group(0) @binding(7) var<uniform> nVertices: u32;

// Complex multiply
fn cmul(a: vec2<f32>, b: vec2<f32>) -> vec2<f32> {
  return vec2<f32>(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

// Point-in-polygon (ray casting)
fn pointInPolygon(p: vec2<f32>, nVerts: u32) -> bool {
  var crossings: u32 = 0u;
  for (var i: u32 = 0u; i < nVerts; i = i + 1u) {
    let v1 = vertices[i];
    let v2 = vertices[(i + 1u) % nVerts];
    if ((v1.y <= p.y && v2.y > p.y) || (v2.y <= p.y && v1.y > p.y)) {
      let t = (p.y - v1.y) / (v2.y - v1.y);
      let xIntersect = v1.x + t * (v2.x - v1.x);
      if (p.x < xIntersect) {
        crossings = crossings + 1u;
      }
    }
  }
  return (crossings % 2u) == 1u;
}

@fragment
fn main(input: VertexOutput) -> @location(0) vec4<f32> {
  let worldPos = params.viewInverse * vec4<f32>(input.uv, 0.0, 1.0);
  let z = worldPos.xy;

  let inside = pointInPolygon(z, nVertices);

  // Smooth part: Σ a_n (z-c)^n
  let dz = z - params.center;
  var power = vec2<f32>(1.0, 0.0);
  var smooth_val = vec2<f32>(0.0, 0.0);
  for (var n: u32 = 0u; n < params.nSmooth; n = n + 1u) {
    smooth_val = smooth_val + cmul(smoothCoeffs[n], power);
    power = cmul(power, dz);
  }

  // Singular part: Σ b_k / (z - p_k) + b0
  var sing_val = params.b0;
  for (var k: u32 = 0u; k < params.nPoles; k = k + 1u) {
    let pole_dz = z - poles[k];
    let d = pole_dz.x * pole_dz.x + pole_dz.y * pole_dz.y;
    let inv = vec2<f32>(pole_dz.x / d, -pole_dz.y / d);
    sing_val = sing_val + cmul(singularCoeffs[k], inv);
  }

  // Full solution: Re(smooth + singular + i*imCorr)
  let w = smooth_val + sing_val + vec2<f32>(0.0, params.imCorr);
  let value = w.x; // Re(w)

  // Map to colorscale
  let t = clamp((value - params.valueMin) / (params.valueMax - params.valueMin), 0.0, 1.0);
  let color = textureSample(colorscale_texture, colorscale_sampler, vec2<f32>(t, 0.5));

  // Contour lines using screen-space derivatives
  let contourInterval = 0.25;

  let du_dx = dpdx(value);
  let du_dy = dpdy(value);
  let u_grad = length(vec2<f32>(du_dx, du_dy));
  let u_dist = abs(fract(value / contourInterval - 0.5) - 0.5) * contourInterval / u_grad;
  let u_alpha = 1.0 - smoothstep(0.5, 1.5, u_dist);

  let v = w.y;
  let dv_dx = dpdx(v);
  let dv_dy = dpdy(v);
  let v_grad = length(vec2<f32>(dv_dx, dv_dy));
  let v_dist = abs(fract(v / contourInterval - 0.5) - 0.5) * contourInterval / v_grad;
  let v_alpha = 1.0 - smoothstep(0.5, 1.5, v_dist);

  if (!inside) {
    return vec4<f32>(1.0, 1.0, 1.0, 1.0);
  }
  let contour_alpha = max(u_alpha, v_alpha);
  let final_color = mix(color.rgb, vec3<f32>(0.0, 0.0, 0.0), contour_alpha * 0.5);
  return vec4<f32>(final_color, 1.0);
}
