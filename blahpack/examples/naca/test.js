// Quick test of the NACA potential flow solver.
import { nacaAirfoil } from './naca.js';
import { potentialFlow } from './potential-flow.js';

console.log('=== NACA Airfoil Geometry ===');
const pts = nacaAirfoil('2412', 100);
console.log(`Points: ${pts.length}`);
console.log(`TE: (${pts[0][0].toFixed(4)}, ${pts[0][1].toFixed(4)})`);
console.log(`LE: (${pts[Math.floor(pts.length/2)][0].toFixed(4)}, ${pts[Math.floor(pts.length/2)][1].toFixed(4)})`);

console.log('\n=== Symmetric NACA 0012 at 0° (should give ~0 lift) ===');
const sol0 = potentialFlow({ digits: '0012', alpha: 0 });
console.log(`CL = ${sol0.CL.toFixed(6)} (expect ~0)`);
console.log(`Max boundary error: ${sol0.maxError.toExponential(2)}`);

console.log('\n=== NACA 0012 at 5° ===');
const sol1 = potentialFlow({ digits: '0012', alpha: 5 });
console.log(`CL = ${sol1.CL.toFixed(4)} (thin airfoil theory ≈ 0.55)`);
console.log(`Max boundary error: ${sol1.maxError.toExponential(2)}`);
console.log(`Far-field speed at (3,0): ${sol1.evaluate([3, 0]).speed.toFixed(4)} (expect ~1.0)`);

console.log('\n=== NACA 2412 at 5° ===');
const sol2 = potentialFlow({ digits: '2412', alpha: 5 });
console.log(`CL = ${sol2.CL.toFixed(4)}`);
console.log(`Γ = ${sol2.Gamma.toFixed(4)}`);
console.log(`Max boundary error: ${sol2.maxError.toExponential(2)}`);

// Cp distribution
console.log('\nCp distribution (upper surface):');
const n = Math.floor(pts.length / 2);
for (let i = 0; i < n; i += Math.floor(n / 8)) {
  const z = sol2.boundary[i];
  const r = sol2.evaluate(z);
  console.log(`  x=${z[0].toFixed(3)} Cp=${r.cp.toFixed(4)}`);
}

console.log('\n=== Angle of attack sweep (NACA 0012) ===');
for (const alpha of [0, 2, 4, 6, 8, 10]) {
  const sol = potentialFlow({ digits: '0012', alpha });
  console.log(`  α=${String(alpha).padStart(2)}°: CL=${sol.CL.toFixed(4)}`);
}
