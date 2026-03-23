// Quick test of the NACA potential flow solver.
import { nacaAirfoil } from './naca.js';
import { potentialFlow } from './potential-flow.js';

console.log('=== NACA Airfoil Geometry ===');
const pts = nacaAirfoil('2412', 100);
console.log(`Points: ${pts.length}`);
console.log(`TE: (${pts[0][0].toFixed(4)}, ${pts[0][1].toFixed(4)})`);
console.log(`LE: (${pts[Math.floor(pts.length/2)][0].toFixed(4)}, ${pts[Math.floor(pts.length/2)][1].toFixed(4)})`);

console.log('\n=== Symmetric airfoil at 0° AoA (should give ~0 lift) ===');
const sol0 = potentialFlow({ digits: '0012', alpha: 0, nPoints: 200, nPoles: 24, nRunge: 16 });
console.log(`Γ = ${sol0.Gamma.toFixed(6)}`);
console.log(`CL = ${sol0.CL.toFixed(6)}`);
console.log(`Max boundary error: ${sol0.maxError.toExponential(2)}`);

console.log('\n=== NACA 2412 at 5° AoA ===');
const sol = potentialFlow({ digits: '2412', alpha: 5, nPoints: 300, nPoles: 30, nRunge: 20 });
console.log(`Γ = ${sol.Gamma.toFixed(6)}`);
console.log(`CL = ${sol.CL.toFixed(6)}`);
console.log(`Max boundary error: ${sol.maxError.toExponential(2)}`);

// Check velocity at a point far from the airfoil
const far = sol.evaluate([10, 0]);
console.log(`\nFar-field velocity: u=${far.u.toFixed(4)}, v=${far.v.toFixed(4)} (expect ~cos(5°), ~sin(5°))`);
console.log(`Far-field speed: ${far.speed.toFixed(4)} (expect ~1.0)`);

// Check Cp distribution on upper/lower surface
const upper = pts.slice(0, Math.floor(pts.length/2));
console.log(`\nCp at LE (upper): ${sol.evaluate(upper[upper.length-1]).cp.toFixed(4)}`);
console.log(`Cp at TE: ${sol.evaluate(pts[0]).cp.toFixed(4)}`);
