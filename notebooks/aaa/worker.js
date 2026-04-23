// Web worker for Laplace/AAA computation.
// Runs the heavy SVD-based computation off the main thread.

import { laplace, pointInPolygon, aaa, timings, stokes } from './laplace-bundle.js';

self.onmessage = function(e) {
  const { type, id } = e.data;

  if (type === 'laplace') {
    const { boundary, boundaryValues, vertices, options } = e.data;
    timings.reset();
    const t0 = performance.now();
    const result = laplace(boundary, boundaryValues, vertices, options);
    const computeTime = performance.now() - t0;
    self.postMessage({
      id, type: 'laplace',
      result: {
        smoothCoeffs: result.smoothCoeffs,
        singularCoeffs: result.singularCoeffs,
        poles: result.poles,
        allPoles: result.allPoles,
        allZeros: result.allZeros,
        b0: result.b0,
        center: result.center,
        imCorr: result.imCorr,
        maxError: result.maxError,
        N: result.N,
        exterior: result.exterior,
        bary: result.bary,
      },
      computeTime,
      timings: { svd: timings.svd, cauchy: timings.cauchy, total: timings.total },
    });
  } else if (type === 'stokes') {
    const { options } = e.data;
    const t0 = performance.now();
    const result = stokes(options);
    const computeTime = performance.now() - t0;
    // Pre-evaluate ψ on a grid (f64 accuracy for the GPU)
    const gridN = options.gridN || 256;
    const margin = 0.01;
    const domMin = [-1 - margin, -1 - margin];
    const domMax = [1 + margin, 1 + margin];
    const psiGrid = new Float32Array(gridN * gridN);
    let psiMin = Infinity, psiMax = -Infinity;
    for (let iy = 0; iy < gridN; iy++) {
      for (let ix = 0; ix < gridN; ix++) {
        const x = domMin[0] + (ix + 0.5) / gridN * (domMax[0] - domMin[0]);
        const y = domMin[1] + (iy + 0.5) / gridN * (domMax[1] - domMin[1]);
        const { psi } = result.evaluate([x, y]);
        psiGrid[iy * gridN + ix] = psi;
        // Only track range for interior points
        const inside = x >= -1 && x <= 1 && y >= -1 && y <= 1;
        if (isFinite(psi) && inside) {
          if (psi < psiMin) psiMin = psi;
          if (psi > psiMax) psiMax = psi;
        }
      }
    }

    self.postMessage({
      id, type: 'stokes',
      result: {
        poles: result.poles,
        corners: result.corners,
        nPoles: result.nPoles,
        nPoly: result.nPoly,
        maxError: result.maxError,
        psiGrid, gridN,
        domMin, domMax,
        psiMin, psiMax,
      },
      computeTime,
    });
  } else if (type === 'aaa') {
    const { Z, F, tol, mmax } = e.data;
    timings.reset();
    const t0 = performance.now();
    const result = aaa(Z, F, tol, mmax);
    const computeTime = performance.now() - t0;
    self.postMessage({
      id, type: 'aaa',
      result: {
        converged: result.converged,
        errvec: result.errvec,
        pol: result.pol,
        zer: result.zer,
      },
      computeTime,
    });
  }
};
