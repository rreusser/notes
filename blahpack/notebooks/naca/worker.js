// Web worker: compute potential flow and pre-evaluate grid for GPU rendering.
import { potentialFlow, nacaAirfoil, squareFlow, exteriorFlow, aaa, pointInPolygon as pipFn } from './naca-bundle.js';

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

function evaluateGrid(sol, bodyVerts, options) {
  const gridN = options.gridN || 512;
  const xMin = options.xMin || -2;
  const xMax = options.xMax || 2;
  const yMin = options.yMin || -2;
  const yMax = options.yMax || 2;

  const psiGrid = new Float32Array(gridN * gridN);
  const maskGrid = new Float32Array(gridN * gridN);

  let psiMin = Infinity, psiMax = -Infinity;

  for (let iy = 0; iy < gridN; iy++) {
    const y = yMin + (yMax - yMin) * iy / (gridN - 1);
    for (let ix = 0; ix < gridN; ix++) {
      const x = xMin + (xMax - xMin) * ix / (gridN - 1);
      const idx = ix + iy * gridN;

      const inside = pointInPolygon(x, y, bodyVerts);
      maskGrid[idx] = inside ? 0.0 : 1.0;

      if (inside) {
        psiGrid[idx] = 0;
        continue;
      }

      const r = sol.evaluate([x, y]);
      const psi = isFinite(r.psi) ? r.psi : 0;
      psiGrid[idx] = psi;

      if (psi < psiMin) psiMin = psi;
      if (psi > psiMax) psiMax = psi;
    }
  }

  // Compute speed from finite differences of ψ: u = ∂ψ/∂y, v = -∂ψ/∂x
  const speedGrid = new Float32Array(gridN * gridN);
  const dx = (xMax - xMin) / (gridN - 1);
  const dy = (yMax - yMin) / (gridN - 1);
  for (let iy = 1; iy < gridN - 1; iy++) {
    for (let ix = 1; ix < gridN - 1; ix++) {
      const idx = ix + iy * gridN;
      if (maskGrid[idx] < 0.5) continue; // inside body
      const dpsi_dy = (psiGrid[ix + (iy+1)*gridN] - psiGrid[ix + (iy-1)*gridN]) / (2*dy);
      const dpsi_dx = (psiGrid[(ix+1) + iy*gridN] - psiGrid[(ix-1) + iy*gridN]) / (2*dx);
      speedGrid[idx] = Math.sqrt(dpsi_dy*dpsi_dy + dpsi_dx*dpsi_dx);
    }
  }

  return { psiGrid, speedGrid, maskGrid, gridN, psiMin, psiMax, domMin: [xMin, yMin], domMax: [xMax, yMax] };
}

self.onmessage = function(e) {
  const { id, type, options } = e.data;

  if (type === 'square') {
    const s = options.halfSide || 0.5;
    const bodyVerts = [[-s,-s],[s,-s],[s,s],[-s,s]];

    // Build boundary points
    const nSample = options.nSample || 300;
    const sides = [[bodyVerts[0],bodyVerts[1]],[bodyVerts[1],bodyVerts[2]],[bodyVerts[2],bodyVerts[3]],[bodyVerts[3],bodyVerts[0]]];
    const boundary = [];
    for (const [st, en] of sides) {
      for (let i = 1; i <= nSample; i++) {
        const t = i / (nSample + 1);
        boundary.push([st[0]+t*(en[0]-st[0]), st[1]+t*(en[1]-st[1])]);
      }
    }

    const t0 = performance.now();
    const sol = exteriorFlow(boundary, bodyVerts, { mmax: options.mmax || 200, aaa, pointInPolygon: pipFn });
    const solveTime = performance.now() - t0;

    const t1 = performance.now();
    const grid = evaluateGrid(sol, bodyVerts, {
      gridN: options.gridN || 512,
      xMin: -3, xMax: 3, yMin: -3, yMax: 3,
    });
    const gridTime = performance.now() - t1;

    self.postMessage({
      id, type,
      result: {
        ...grid,
        boundary: bodyVerts.concat([bodyVerts[0]]),
        poles: sol.poles,
        maxError: sol.maxError,
      },
      solveTime, gridTime,
    });
  }

  if (type === 'naca') {
    const digits = options.digits || '2412';
    const nPoints = options.nPoints || 400;
    const boundary = nacaAirfoil(digits, nPoints);

    const t0 = performance.now();
    // Trailing edge is the first boundary point (sharp corner)
    const te = boundary[0];
    const sol = exteriorFlow(boundary, boundary, {
      mmax: options.mmax || 200,
      N: 10,
      minPoleDist: 0.005,
      corners: [te],
      cornerRadius: 0.15,
      kutta: true,
      trailingEdge: te,
      aaa,
      pointInPolygon: pipFn,
    });
    const solveTime = performance.now() - t0;

    const t1 = performance.now();
    const grid = evaluateGrid(sol, boundary, {
      gridN: options.gridN || 512,
      xMin: -0.5, xMax: 1.5, yMin: -0.6, yMax: 0.6,
    });
    const gridTime = performance.now() - t1;

    self.postMessage({
      id, type,
      result: {
        ...grid,
        boundary,
        poles: sol.poles,
        smoothCoeffs: sol.smoothCoeffs,
        singularCoeffs: sol.singularCoeffs,
        center: sol.center,
        Gamma: sol.Gamma || 0,
        CL: sol.CL || 0,
        maxError: sol.maxError,
        digits,
      },
      solveTime, gridTime,
    });
  }
};
