// Web worker: compute NACA potential flow and pre-evaluate grid for GPU rendering.
import { potentialFlow, nacaAirfoil } from './naca-bundle.js';

// Ray-casting point-in-polygon
function pointInPolygon(px, py, verts) {
  let crossings = 0;
  const n = verts.length;
  for (let i = 0; i < n; i++) {
    const [x1, y1] = verts[i];
    const [x2, y2] = verts[(i + 1) % n];
    if ((y1 <= py && y2 > py) || (y2 <= py && y1 > py)) {
      const t = (py - y1) / (y2 - y1);
      if (px < x1 + t * (x2 - x1)) crossings++;
    }
  }
  return (crossings % 2) === 1;
}

self.onmessage = function(e) {
  const { id, type, options } = e.data;

  if (type === 'naca') {
    const t0 = performance.now();
    const sol = potentialFlow(options);
    const solveTime = performance.now() - t0;

    const gridN = options.gridN || 512;
    const pad = 0.5;
    const xMin = -pad, xMax = 1 + pad;
    const yMin = -0.8, yMax = 0.8;

    const psiGrid = new Float32Array(gridN * gridN);
    const speedGrid = new Float32Array(gridN * gridN);
    const maskGrid = new Float32Array(gridN * gridN); // 1=outside, 0=inside airfoil

    let psiMin = Infinity, psiMax = -Infinity;
    const U = options.U || 1;

    // Precompute min distance to boundary for each grid point (approximate)
    // Use a subset of boundary points for speed
    const bdy = sol.boundary;
    const bdyStep = Math.max(1, Math.floor(bdy.length / 100));

    const t1 = performance.now();
    for (let iy = 0; iy < gridN; iy++) {
      const y = yMin + (yMax - yMin) * iy / (gridN - 1);
      for (let ix = 0; ix < gridN; ix++) {
        const x = xMin + (xMax - xMin) * ix / (gridN - 1);
        const idx = ix + iy * gridN;

        const inside = pointInPolygon(x, y, bdy);

        // Approximate distance to boundary
        let minDist2 = Infinity;
        for (let k = 0; k < bdy.length; k += bdyStep) {
          const dx = x - bdy[k][0], dy = y - bdy[k][1];
          const d2 = dx * dx + dy * dy;
          if (d2 < minDist2) minDist2 = d2;
        }
        const minDist = Math.sqrt(minDist2);

        // Mask: 0 inside, ramp from 0→1 in a thin layer near the surface
        const margin = 0.015; // mask margin thickness
        if (inside) {
          maskGrid[idx] = 0.0;
        } else {
          maskGrid[idx] = Math.min(1.0, minDist / margin);
        }

        if (inside || minDist < margin * 0.5) {
          psiGrid[idx] = 0;
          speedGrid[idx] = 0;
          continue;
        }

        const r = sol.evaluate([x, y]);
        const psi = isFinite(r.psi) ? r.psi : 0;
        const speed = isFinite(r.speed) ? Math.min(r.speed, 5 * U) : 0;

        psiGrid[idx] = psi;
        speedGrid[idx] = speed;

        if (psi < psiMin) psiMin = psi;
        if (psi > psiMax) psiMax = psi;
      }
    }
    const gridTime = performance.now() - t1;

    // Clamp psi range symmetrically for nice contours
    const psiAbsMax = Math.max(Math.abs(psiMin), Math.abs(psiMax));
    psiMin = -psiAbsMax;
    psiMax = psiAbsMax;

    self.postMessage({
      id, type,
      result: {
        psiGrid, speedGrid, maskGrid,
        gridN,
        domMin: [xMin, yMin],
        domMax: [xMax, yMax],
        psiMin, psiMax,
        speedMax: 2.0 * U, // normalize so freestream is at 0.5
        boundary: sol.boundary,
        poles: sol.poles,
        CL: sol.CL,
        Gamma: sol.Gamma,
        maxError: sol.maxError,
        digits: sol.digits,
        alpha: sol.alpha,
      },
      solveTime,
      gridTime,
    });
  }
};
