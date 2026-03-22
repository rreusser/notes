#!/usr/bin/env node
// Compare Fortran (reference LAPACK) vs JavaScript (blahpack) AAA convergence

import { aaa } from '../../aaa-pure.js';
import { readFileSync } from 'fs';

const lines = readFileSync('test/fixtures/aaa.jsonl', 'utf8').trim().split('\n');
const fortranExp = JSON.parse(lines[0]);
const fortranL = JSON.parse(lines[1]);

// ---- Test 1: exp(z) on unit circle ----
const M = 200;
const Z = [], F = [];
for (let i = 0; i < M; i++) {
  const theta = 2 * Math.PI * i / M;
  const cr = Math.cos(theta), ci = Math.sin(theta);
  Z.push([cr, ci]);
  F.push([Math.exp(cr)*Math.cos(ci), Math.exp(cr)*Math.sin(ci)]);
}
const r1 = aaa(Z, F, 1e-13, 100);

console.log('=== exp(z) on unit circle ===');
console.log('iter  Fortran(LAPACK)    JavaScript(blahpack)');
console.log('----  ----------------   --------------------');
const n = Math.max(fortranExp.niters, r1.errvec.length);
for (let i = 0; i < n; i++) {
  const fe = i < fortranExp.niters ? fortranExp.errvec[i].toExponential(3) : '';
  const je = i < r1.errvec.length ? r1.errvec[i].toExponential(3) : '';
  console.log(`${(i+1).toString().padStart(4)}  ${fe.padStart(16)}   ${je.padStart(16)}`);
}

// ---- Test 2: x^2 on L-shaped boundary ----
function linspace(a, b, step) {
  const arr = [];
  if (a <= b) for (let x = a; x <= b + step/2; x += step) arr.push(x);
  else for (let x = a; x >= b - step/2; x -= step) arr.push(x);
  return arr;
}
const boundary = [];
for (const x of linspace(0, 2, 0.01)) boundary.push([x, 0]);
for (const y of linspace(0, 1, 0.01)) boundary.push([2, y]);
for (const x of linspace(2, 1, 0.01)) boundary.push([x, 1]);
for (const y of linspace(1, 2, 0.01)) boundary.push([1, y]);
for (const x of linspace(1, 0, 0.01)) boundary.push([x, 2]);
for (const y of linspace(2, 0, 0.01)) boundary.push([0, y]);

const Z2 = boundary.map(p => p);
const F2 = boundary.map(([x]) => [x*x, 0]);
const r2 = aaa(Z2, F2, 1e-13, 100);

console.log(`\n=== x^2 on L-shaped boundary ===`);
console.log(`Fortran npts: ${fortranL.npts}, JS npts: ${boundary.length}`);
console.log('iter  Fortran(LAPACK)    JavaScript(blahpack)');
console.log('----  ----------------   --------------------');
const n2 = Math.max(fortranL.niters, r2.errvec.length);
for (let i = 0; i < n2; i += 5) {
  const fe = i < fortranL.niters ? fortranL.errvec[i].toExponential(3) : '';
  const je = i < r2.errvec.length ? r2.errvec[i].toExponential(3) : '';
  console.log(`${(i+1).toString().padStart(4)}  ${fe.padStart(16)}   ${je.padStart(16)}`);
}
const fi = fortranL.niters - 1;
const ji = r2.errvec.length - 1;
console.log(`\nFinal: Fortran iter ${fortranL.niters}: ${fortranL.errvec[fi].toExponential(3)}  JS iter ${r2.errvec.length}: ${r2.errvec[ji].toExponential(3)}`);
