// NACA 4-digit airfoil geometry.
// Reference: Abbott & von Doenhoff, "Theory of Wing Sections", 1959.
//
// NACA MPXX:
//   M = maximum camber (% of chord)
//   P = position of max camber (tenths of chord)
//   XX = maximum thickness (% of chord)

// Thickness distribution for a NACA 4-digit airfoil at chord fraction x ∈ [0,1].
// Returns half-thickness (unsigned).
function thickness(x, t) {
  return 5 * t * (
    0.2969 * Math.sqrt(x)
    - 0.1260 * x
    - 0.3516 * x * x
    + 0.2843 * x * x * x
    - 0.1015 * x * x * x * x  // closed trailing edge (use -0.1036 for open)
  );
}

// Camber line y_c(x) and its derivative dy_c/dx for NACA 4-digit.
function camber(x, m, p) {
  if (m === 0) return { yc: 0, dyc: 0 };
  if (x < p) {
    return {
      yc: (m / (p * p)) * (2 * p * x - x * x),
      dyc: (2 * m / (p * p)) * (p - x),
    };
  }
  return {
    yc: (m / ((1 - p) * (1 - p))) * (1 - 2 * p + 2 * p * x - x * x),
    dyc: (2 * m / ((1 - p) * (1 - p))) * (p - x),
  };
}

// Generate NACA 4-digit airfoil boundary points.
// digits: string like "2412" or "0012"
// nPoints: total number of boundary points (half upper, half lower)
// Returns array of [x, y] points going clockwise from trailing edge
// along the upper surface, around the leading edge, back along the lower
// surface to the trailing edge.
export function nacaAirfoil(digits, nPoints = 200) {
  const M = parseInt(digits[0]) / 100;   // max camber
  const P = parseInt(digits[1]) / 10;    // position of max camber
  const T = parseInt(digits.slice(2)) / 100; // thickness

  const n = Math.floor(nPoints / 2);
  const upper = [];
  const lower = [];

  // Cosine spacing for better resolution at leading/trailing edges
  for (let i = 0; i <= n; i++) {
    const beta = Math.PI * i / n;
    const xc = 0.5 * (1 - Math.cos(beta));

    const yt = thickness(xc, T);
    const { yc, dyc } = camber(xc, M, P || 0.01);
    const theta = Math.atan(dyc);

    upper.push([xc - yt * Math.sin(theta), yc + yt * Math.cos(theta)]);
    lower.push([xc + yt * Math.sin(theta), yc - yt * Math.cos(theta)]);
  }

  // Combine: upper surface TE → LE, then lower surface LE → TE.
  // Skip duplicate at LE (both surfaces share it).
  // Start and end at the trailing edge (the corner where Kutta applies).
  const points = [];
  // Upper: from TE (i=n) backwards to LE (i=0)
  for (let i = n; i >= 0; i--) points.push(upper[i]);
  // Lower: from LE (i=1, skip LE duplicate) to TE (i=n-1, skip TE duplicate)
  for (let i = 1; i < n; i++) points.push(lower[i]);

  return points;
}

// Compute the trailing edge angle (interior angle in radians).
// For a closed NACA airfoil this is ~12.5° ≈ 0.22 rad.
export function trailingEdgeAngle(digits) {
  const T = parseInt(digits.slice(2)) / 100;
  // Upper/lower surface slopes at x=1
  const dx = 1e-6;
  const ytu = thickness(1, T);
  const ytd = thickness(1 - dx, T);
  const slope = (ytu - ytd) / dx;
  // Interior angle = π - 2*arctan(slope) for symmetric airfoil
  return Math.PI - 2 * Math.atan(Math.abs(slope));
}

// Compute the outward bisector angle at the trailing edge for pole placement.
// Returns the angle (in radians) pointing away from the airfoil at the TE.
export function trailingEdgeBisector(points) {
  // Last point and first point are near the TE.
  // Bisector points along the chord direction (outward from TE).
  const n = points.length;
  const p0 = points[0];            // upper surface near TE
  const p1 = points[1];            // next upper point
  const pn1 = points[n - 1];       // lower surface near TE
  const pn2 = points[n - 2];       // next lower point

  // Tangent vectors pointing away from TE along each surface
  const t1 = [p1[0] - p0[0], p1[1] - p0[1]];
  const t2 = [pn2[0] - pn1[0], pn2[1] - pn1[1]];

  // Bisector is average of the two tangent directions, then negate (outward)
  const bx = -(t1[0] + t2[0]);
  const by = -(t1[1] + t2[1]);

  return Math.atan2(by, bx);
}
