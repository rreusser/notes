// Web worker for Laplace/AAA computation.
// Runs the heavy SVD-based computation off the main thread.

import { laplace, pointInPolygon, aaa, timings } from './laplace-bundle.js';

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
