#!/usr/bin/env node
// Generate pre-computed Laplace solver data for the visualization notebook.
// Run from project root: node notebooks/laplace/generate-data.js

import { laplace, pointInPolygon } from '../../examples/laplace/laplace.js';
import { aaa } from '../../aaa-pure.js';
import { writeFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function linspace(a, b, step) {
  const arr = [];
  if (a <= b) for (let x = a; x <= b + step / 2; x += step) arr.push(x);
  else for (let x = a; x >= b - step / 2; x -= step) arr.push(x);
  return arr;
}

function buildLShapedBoundary(stp) {
  const boundary = [];
  for (const x of linspace(0, 2, stp)) boundary.push([x, 0]);
  for (const y of linspace(0, 1, stp)) boundary.push([2, y]);
  for (const x of linspace(2, 1, stp)) boundary.push([x, 1]);
  for (const y of linspace(1, 2, stp)) boundary.push([1, y]);
  for (const x of linspace(1, 0, stp)) boundary.push([x, 2]);
  for (const y of linspace(2, 0, stp)) boundary.push([0, y]);
  return boundary;
}

function unitCircle(M) {
  const Z = [], F_exp = [];
  for (let i = 0; i < M; i++) {
    const theta = 2 * Math.PI * i / M;
    const cr = Math.cos(theta), ci = Math.sin(theta);
    Z.push([cr, ci]);
    F_exp.push([Math.exp(cr) * Math.cos(ci), Math.exp(cr) * Math.sin(ci)]);
  }
  return { Z, F_exp };
}

const cases = [];

// ---- Case 1: L-shaped domain, u(z) = x² ----
console.log('Computing L-shaped domain, u = x²...');
const vertices = [[0, 0], [2, 0], [2, 1], [1, 1], [1, 2], [0, 2]];
const boundary = buildLShapedBoundary(0.01);
const bvX2 = boundary.map(([x]) => x * x);
const laplaceX2 = laplace(boundary, bvX2, vertices, {
  center: [0.5, 0.5],
  interior: true,
  mmax: 100,
});
cases.push({
  name: 'L-shaped, u = x²',
  description: 'Laplace equation on L-shaped domain with Dirichlet BC u(z) = x²',
  vertices,
  ...laplaceX2,
  evaluate: undefined, // can't serialize functions
  valueRange: [0, 4],
});

// ---- Case 2: L-shaped domain, u(z) = Re(z²) = x² - y² ----
console.log('Computing L-shaped domain, u = x² - y²...');
const bvX2Y2 = boundary.map(([x, y]) => x * x - y * y);
const laplaceX2Y2 = laplace(boundary, bvX2Y2, vertices, {
  center: [0.5, 0.5],
  interior: true,
  mmax: 100,
});
cases.push({
  name: 'L-shaped, u = x² − y²',
  description: 'Laplace equation on L-shaped domain with Dirichlet BC u(z) = Re(z²)',
  vertices,
  ...laplaceX2Y2,
  evaluate: undefined,
  valueRange: [-4, 4],
});

// ---- Case 3: L-shaped domain, u(z) = Re(exp(z)) ----
console.log('Computing L-shaped domain, u = Re(exp(z))...');
const bvExp = boundary.map(([x, y]) => Math.exp(x) * Math.cos(y));
const laplaceExp = laplace(boundary, bvExp, vertices, {
  center: [0.5, 0.5],
  interior: true,
  mmax: 100,
});
cases.push({
  name: 'L-shaped, u = Re(eᶻ)',
  description: 'Laplace equation on L-shaped domain with Dirichlet BC u(z) = Re(exp(z))',
  vertices,
  ...laplaceExp,
  evaluate: undefined,
  valueRange: [Math.min(...bvExp), Math.max(...bvExp)],
});

// ---- AAA convergence data for exp(z) on unit circle ----
console.log('Computing AAA convergence for exp(z)...');
const { Z, F_exp } = unitCircle(200);
const aaaResult = aaa(Z, F_exp, 1e-13, 50);
cases.push({
  name: 'AAA: exp(z) on unit circle',
  description: 'AAA rational approximation of exp(z) on 200 unit circle points',
  convergence: {
    errvec: aaaResult.errvec,
    nPoles: aaaResult.pol.length,
    nZeros: aaaResult.zer.length,
    converged: aaaResult.converged,
    poles: aaaResult.pol,
    zeros: aaaResult.zer,
  },
});

const outPath = join(__dirname, 'data.json');
writeFileSync(outPath, JSON.stringify(cases, null, 2));
console.log(`Wrote ${cases.length} test cases to ${outPath}`);
