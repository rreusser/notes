// Browser-compatible entry point for potential flow solvers.
export { potentialFlow } from '../../examples/naca/potential-flow.js';
export { nacaAirfoil } from '../../examples/naca/naca.js';
export { squareFlow } from '../../examples/naca/square-flow.js';
export { exteriorFlow } from '../../examples/naca/exterior-flow.js';

// AAA from the browser-compatible entry (uses ESM imports, not createRequire)
export { aaa } from '../aaa/aaa-entry.js';

// Inline pointInPolygon to avoid pulling in laplace.js → aaa-pure.js
export function pointInPolygon([px, py], vertices) {
  let crossings = 0;
  const n = vertices.length;
  for (let i = 0; i < n; i++) {
    const [x1, y1] = vertices[i];
    const [x2, y2] = vertices[(i + 1) % n];
    if ((y1 <= py && y2 > py) || (y2 <= py && y1 > py)) {
      const t = (py - y1) / (y2 - y1);
      if (px < x1 + t * (x2 - x1)) crossings++;
    }
  }
  return crossings % 2 === 1;
}
