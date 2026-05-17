'use strict';
var Float64Array = require('@stdlib/array/float64');
var dorbdb1 = require('/Users/rreusser/gh/rreusser/notes/lib/lapack/base/dorbdb1/lib/ndarray.js');

var LDX = 16;

function buildInputs(P, MP, Q, seed) {
  var Qe = Q < 1 ? 1 : Q;
  var X11 = new Float64Array(LDX * Qe);
  var X21 = new Float64Array(LDX * Qe);
  var M = P + MP;
  var A = new Float64Array(M * Qe);
  var i, j, k, pass, dot;
  for (j = 0; j < Qe; j++) for (i = 0; i < M; i++) A[i + j*M] = Math.sin((i+1)*17 + (j+1)*31 + seed*7);
  for (pass = 0; pass < 2; pass++) {
    for (j = 0; j < Qe; j++) {
      for (i = 0; i < j; i++) {
        dot = 0; for (k = 0; k < M; k++) dot += A[k + i*M] * A[k + j*M];
        for (k = 0; k < M; k++) A[k + j*M] -= dot * A[k + i*M];
      }
      dot = 0; for (k = 0; k < M; k++) dot += A[k + j*M] * A[k + j*M];
      dot = Math.sqrt(dot);
      for (k = 0; k < M; k++) A[k + j*M] /= dot;
    }
  }
  for (j = 0; j < Qe; j++) {
    for (i = 0; i < P; i++) X11[i + j*LDX] = A[i + j*M];
    for (i = 0; i < MP; i++) X21[i + j*LDX] = A[(P+i) + j*M];
  }
  return {X11: X11, X21: X21};
}

var ins = buildInputs(5, 5, 3, 2);
console.log('X11 col 0:', Array.from(ins.X11.slice(0, 5)));
console.log('X11 col 1:', Array.from(ins.X11.slice(LDX, LDX+5)));
console.log('X11 col 2:', Array.from(ins.X11.slice(2*LDX, 2*LDX+5)));
console.log('X21 col 0:', Array.from(ins.X21.slice(0, 5)));
console.log('X21 col 1:', Array.from(ins.X21.slice(LDX, LDX+5)));
console.log('X21 col 2:', Array.from(ins.X21.slice(2*LDX, 2*LDX+5)));

var THETA = new Float64Array(LDX);
var PHI = new Float64Array(LDX);
var TAUP1 = new Float64Array(LDX);
var TAUP2 = new Float64Array(LDX);
var TAUQ1 = new Float64Array(LDX);
var WORK = new Float64Array(LDX*LDX);

dorbdb1(10, 5, 3, ins.X11, 1, LDX, 0, ins.X21, 1, LDX, 0, THETA, 1, 0, PHI, 1, 0, TAUP1, 1, 0, TAUP2, 1, 0, TAUQ1, 1, 0, WORK, 1, 0);

console.log('THETA:', Array.from(THETA.slice(0, 3)));
console.log('PHI:', Array.from(PHI.slice(0, 2)));
console.log('TAUP1:', Array.from(TAUP1.slice(0, 5)));
console.log('TAUP2:', Array.from(TAUP2.slice(0, 5)));
console.log('TAUQ1:', Array.from(TAUQ1.slice(0, 3)));
