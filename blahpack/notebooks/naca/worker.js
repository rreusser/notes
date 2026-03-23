// Web worker: compute NACA potential flow and pre-evaluate grid for GPU rendering.
import { potentialFlow, nacaAirfoil } from './naca-bundle.js';

self.onmessage = function(e) {
  const { id, type, options } = e.data;

  if (type === 'naca') {
    const t0 = performance.now();
    const sol = potentialFlow(options);
    const solveTime = performance.now() - t0;

    // Pre-evaluate on a grid for texture-based rendering.
    const gridN = options.gridN || 512;
    const pad = 0.5;
    const xMin = -pad, xMax = 1 + pad;
    const yMin = -0.8, yMax = 0.8;

    const psiGrid = new Float32Array(gridN * gridN);
    const speedGrid = new Float32Array(gridN * gridN);
    const cpGrid = new Float32Array(gridN * gridN);

    let psiMin = Infinity, psiMax = -Infinity;
    let speedMax = 0;

    const t1 = performance.now();
    for (let iy = 0; iy < gridN; iy++) {
      const y = yMin + (yMax - yMin) * iy / (gridN - 1);
      for (let ix = 0; ix < gridN; ix++) {
        const x = xMin + (xMax - xMin) * ix / (gridN - 1);
        const idx = ix + iy * gridN;

        const r = sol.evaluate([x, y]);
        psiGrid[idx] = r.psi;
        speedGrid[idx] = r.speed;
        cpGrid[idx] = r.cp;

        if (isFinite(r.psi)) {
          if (r.psi < psiMin) psiMin = r.psi;
          if (r.psi > psiMax) psiMax = r.psi;
        }
        if (isFinite(r.speed) && r.speed > speedMax) speedMax = r.speed;
      }
    }
    const gridTime = performance.now() - t1;

    // Clamp psi range for visualization (avoid extreme outliers near z*)
    const psiRange = psiMax - psiMin;
    psiMin = Math.max(psiMin, -2);
    psiMax = Math.min(psiMax, 2);

    self.postMessage({
      id, type,
      result: {
        psiGrid, speedGrid, cpGrid,
        gridN,
        domMin: [xMin, yMin],
        domMax: [xMax, yMax],
        psiMin, psiMax, speedMax,
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
