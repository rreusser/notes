'use strict';

/* eslint-disable max-len, max-params, max-statements, max-depth */

// v3: 4x4 register-tiled micro-kernel (dot-product form) for contiguous
// column-major NN. A 4x4 block of C is accumulated entirely in registers
// across the full K loop, so C is touched once (vs. K times in the axpy form)
// and each A/B load is reused 4x. Edge rows/cols use scalar dot products.

var ref = require( './v0-reference.js' );

function dgemm( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc ) {
	var c00, c01, c02, c03, c10, c11, c12, c13, c20, c21, c22, c23, c30, c31, c32, c33;
	var a0, a1, a2, a3, b0, b1, b2, b3;
	var paBase, pa;
	var pb0, pb1, pb2, pb3, pb;
	var pcCol, pc;
	var i, j, l, jj, ii;
	var mb, nb;
	var temp;

	if ( M === 0 || N === 0 || ( ( alpha === 0.0 || K === 0 ) && beta === 1.0 ) ) {
		return C;
	}
	if ( transa !== 'no-transpose' || transb !== 'no-transpose' || sa1 !== 1 || sc1 !== 1 || alpha === 0.0 ) {
		return ref( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc );
	}

	mb = M - ( M % 4 );
	nb = N - ( N % 4 );

	// Main 4x4 tiles:
	for ( j = 0; j < nb; j += 4 ) {
		pb0 = ob + ( j * sb2 );
		pb1 = pb0 + sb2;
		pb2 = pb1 + sb2;
		pb3 = pb2 + sb2;
		for ( i = 0; i < mb; i += 4 ) {
			c00 = c01 = c02 = c03 = 0.0;
			c10 = c11 = c12 = c13 = 0.0;
			c20 = c21 = c22 = c23 = 0.0;
			c30 = c31 = c32 = c33 = 0.0;
			for ( l = 0; l < K; l++ ) {
				paBase = oa + i + ( l * sa2 );
				a0 = A[ paBase ];
				a1 = A[ paBase + 1 ];
				a2 = A[ paBase + 2 ];
				a3 = A[ paBase + 3 ];
				b0 = B[ pb0 + ( l * sb1 ) ];
				b1 = B[ pb1 + ( l * sb1 ) ];
				b2 = B[ pb2 + ( l * sb1 ) ];
				b3 = B[ pb3 + ( l * sb1 ) ];
				c00 += a0 * b0; c10 += a1 * b0; c20 += a2 * b0; c30 += a3 * b0;
				c01 += a0 * b1; c11 += a1 * b1; c21 += a2 * b1; c31 += a3 * b1;
				c02 += a0 * b2; c12 += a1 * b2; c22 += a2 * b2; c32 += a3 * b2;
				c03 += a0 * b3; c13 += a1 * b3; c23 += a2 * b3; c33 += a3 * b3;
			}
			// Write back: C = alpha*acc + beta*C
			pcCol = oc + i + ( j * sc2 );
			if ( beta === 0.0 ) {
				C[pcCol]=alpha*c00; C[pcCol+1]=alpha*c10; C[pcCol+2]=alpha*c20; C[pcCol+3]=alpha*c30;
				pc=pcCol+sc2; C[pc]=alpha*c01; C[pc+1]=alpha*c11; C[pc+2]=alpha*c21; C[pc+3]=alpha*c31;
				pc+=sc2; C[pc]=alpha*c02; C[pc+1]=alpha*c12; C[pc+2]=alpha*c22; C[pc+3]=alpha*c32;
				pc+=sc2; C[pc]=alpha*c03; C[pc+1]=alpha*c13; C[pc+2]=alpha*c23; C[pc+3]=alpha*c33;
			} else {
				C[pcCol]=alpha*c00+beta*C[pcCol]; C[pcCol+1]=alpha*c10+beta*C[pcCol+1]; C[pcCol+2]=alpha*c20+beta*C[pcCol+2]; C[pcCol+3]=alpha*c30+beta*C[pcCol+3];
				pc=pcCol+sc2; C[pc]=alpha*c01+beta*C[pc]; C[pc+1]=alpha*c11+beta*C[pc+1]; C[pc+2]=alpha*c21+beta*C[pc+2]; C[pc+3]=alpha*c31+beta*C[pc+3];
				pc+=sc2; C[pc]=alpha*c02+beta*C[pc]; C[pc+1]=alpha*c12+beta*C[pc+1]; C[pc+2]=alpha*c22+beta*C[pc+2]; C[pc+3]=alpha*c32+beta*C[pc+3];
				pc+=sc2; C[pc]=alpha*c03+beta*C[pc]; C[pc+1]=alpha*c13+beta*C[pc+1]; C[pc+2]=alpha*c23+beta*C[pc+2]; C[pc+3]=alpha*c33+beta*C[pc+3];
			}
		}
	}

	// Edge: remaining rows (i >= mb) for all columns, and remaining cols for all rows.
	// Handle leftover columns [nb, N) over all rows, and leftover rows [mb, M) over cols [0, nb).
	// Scalar dot-product form, C touched once.
	for ( jj = nb; jj < N; jj++ ) {
		pb = ob + ( jj * sb2 );
		for ( ii = 0; ii < M; ii++ ) {
			temp = 0.0;
			pa = oa + ii;
			for ( l = 0; l < K; l++ ) {
				temp += A[ pa + ( l * sa2 ) ] * B[ pb + ( l * sb1 ) ];
			}
			pc = oc + ii + ( jj * sc2 );
			C[ pc ] = ( beta === 0.0 ) ? alpha*temp : alpha*temp + beta*C[ pc ];
		}
	}
	for ( jj = 0; jj < nb; jj++ ) {
		pb = ob + ( jj * sb2 );
		for ( ii = mb; ii < M; ii++ ) {
			temp = 0.0;
			pa = oa + ii;
			for ( l = 0; l < K; l++ ) {
				temp += A[ pa + ( l * sa2 ) ] * B[ pb + ( l * sb1 ) ];
			}
			pc = oc + ii + ( jj * sc2 );
			C[ pc ] = ( beta === 0.0 ) ? alpha*temp : alpha*temp + beta*C[ pc ];
		}
	}
	return C;
}

module.exports = dgemm;
