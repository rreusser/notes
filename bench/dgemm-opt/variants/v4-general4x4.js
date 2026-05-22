'use strict';

/* eslint-disable max-len, max-params, max-statements, max-depth */

// v4: General-stride 4x4 register-tiled kernel. Unlike v1-v3 (which only
// accelerate contiguous column-major NN and fall back otherwise), this handles
// ALL transpose combinations and ALL layouts by expressing access in terms of
// effective row/col strides:
//
//   op(A)[m,k] : ar = nota ? sa1 : sa2  (per-row, m) ; ak = nota ? sa2 : sa1 (per-k)
//   op(B)[k,n] : bk = notb ? sb1 : sb2  (per-k)       ; bn = notb ? sb2 : sb1 (per-n)
//   C[m,n]     : oc + m*sc1 + n*sc2
//
// A 4x4 block of C is accumulated in registers across the whole K loop, so C is
// touched once and each A/B load is reused 4x regardless of stride pattern.

function dgemm( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc ) {
	var nota, notb, ar, ak, bk, bn;
	var c00, c01, c02, c03, c10, c11, c12, c13, c20, c21, c22, c23, c30, c31, c32, c33;
	var a0, a1, a2, a3, b0, b1, b2, b3;
	var pa, pa0, pa1, pa2, pa3, pb0, pb1, pb2, pb3, pc, pcc, pb, pak;
	var i, j, l, jj, ii, mb, nb, temp;

	if ( M === 0 || N === 0 || ( ( alpha === 0.0 || K === 0 ) && beta === 1.0 ) ) {
		return C;
	}
	nota = ( transa === 'no-transpose' );
	notb = ( transb === 'no-transpose' );
	ar = nota ? sa1 : sa2;
	ak = nota ? sa2 : sa1;
	bk = notb ? sb1 : sb2;
	bn = notb ? sb2 : sb1;

	// alpha === 0: just scale C by beta.
	if ( alpha === 0.0 ) {
		for ( j = 0; j < N; j++ ) {
			pc = oc + ( j * sc2 );
			if ( beta === 0.0 ) {
				for ( i = 0; i < M; i++ ) { C[ pc ] = 0.0; pc += sc1; }
			} else {
				for ( i = 0; i < M; i++ ) { C[ pc ] *= beta; pc += sc1; }
			}
		}
		return C;
	}

	mb = M - ( M % 4 );
	nb = N - ( N % 4 );

	for ( j = 0; j < nb; j += 4 ) {
		pb0 = ob + ( j * bn );
		pb1 = pb0 + bn;
		pb2 = pb1 + bn;
		pb3 = pb2 + bn;
		for ( i = 0; i < mb; i += 4 ) {
			c00 = 0.0; c10 = 0.0; c20 = 0.0; c30 = 0.0;
			c01 = 0.0; c11 = 0.0; c21 = 0.0; c31 = 0.0;
			c02 = 0.0; c12 = 0.0; c22 = 0.0; c32 = 0.0;
			c03 = 0.0; c13 = 0.0; c23 = 0.0; c33 = 0.0;
			pa0 = oa + ( i * ar );
			pa1 = pa0 + ar;
			pa2 = pa1 + ar;
			pa3 = pa2 + ar;
			for ( l = 0; l < K; l++ ) {
				pak = l * ak;
				a0 = A[ pa0 + pak ];
				a1 = A[ pa1 + pak ];
				a2 = A[ pa2 + pak ];
				a3 = A[ pa3 + pak ];
				b0 = B[ pb0 + ( l * bk ) ];
				b1 = B[ pb1 + ( l * bk ) ];
				b2 = B[ pb2 + ( l * bk ) ];
				b3 = B[ pb3 + ( l * bk ) ];
				c00 += a0 * b0; c10 += a1 * b0; c20 += a2 * b0; c30 += a3 * b0;
				c01 += a0 * b1; c11 += a1 * b1; c21 += a2 * b1; c31 += a3 * b1;
				c02 += a0 * b2; c12 += a1 * b2; c22 += a2 * b2; c32 += a3 * b2;
				c03 += a0 * b3; c13 += a1 * b3; c23 += a2 * b3; c33 += a3 * b3;
			}
			pc = oc + ( i * sc1 ) + ( j * sc2 );
			if ( beta === 0.0 ) {
				pcc = pc; C[pcc]=alpha*c00; C[pcc+sc1]=alpha*c10; C[pcc+2*sc1]=alpha*c20; C[pcc+3*sc1]=alpha*c30;
				pcc=pc+sc2; C[pcc]=alpha*c01; C[pcc+sc1]=alpha*c11; C[pcc+2*sc1]=alpha*c21; C[pcc+3*sc1]=alpha*c31;
				pcc=pc+2*sc2; C[pcc]=alpha*c02; C[pcc+sc1]=alpha*c12; C[pcc+2*sc1]=alpha*c22; C[pcc+3*sc1]=alpha*c32;
				pcc=pc+3*sc2; C[pcc]=alpha*c03; C[pcc+sc1]=alpha*c13; C[pcc+2*sc1]=alpha*c23; C[pcc+3*sc1]=alpha*c33;
			} else {
				pcc=pc; C[pcc]=alpha*c00+beta*C[pcc]; C[pcc+sc1]=alpha*c10+beta*C[pcc+sc1]; C[pcc+2*sc1]=alpha*c20+beta*C[pcc+2*sc1]; C[pcc+3*sc1]=alpha*c30+beta*C[pcc+3*sc1];
				pcc=pc+sc2; C[pcc]=alpha*c01+beta*C[pcc]; C[pcc+sc1]=alpha*c11+beta*C[pcc+sc1]; C[pcc+2*sc1]=alpha*c21+beta*C[pcc+2*sc1]; C[pcc+3*sc1]=alpha*c31+beta*C[pcc+3*sc1];
				pcc=pc+2*sc2; C[pcc]=alpha*c02+beta*C[pcc]; C[pcc+sc1]=alpha*c12+beta*C[pcc+sc1]; C[pcc+2*sc1]=alpha*c22+beta*C[pcc+2*sc1]; C[pcc+3*sc1]=alpha*c32+beta*C[pcc+3*sc1];
				pcc=pc+3*sc2; C[pcc]=alpha*c03+beta*C[pcc]; C[pcc+sc1]=alpha*c13+beta*C[pcc+sc1]; C[pcc+2*sc1]=alpha*c23+beta*C[pcc+2*sc1]; C[pcc+3*sc1]=alpha*c33+beta*C[pcc+3*sc1];
			}
		}
	}
	// Edge cols [nb,N) over all rows:
	for ( jj = nb; jj < N; jj++ ) {
		pb = ob + ( jj * bn );
		for ( ii = 0; ii < M; ii++ ) {
			temp = 0.0;
			pa = oa + ( ii * ar );
			for ( l = 0; l < K; l++ ) { temp += A[ pa + ( l*ak ) ] * B[ pb + ( l*bk ) ]; }
			pc = oc + ( ii*sc1 ) + ( jj*sc2 );
			C[ pc ] = ( beta === 0.0 ) ? alpha*temp : ( alpha*temp ) + ( beta*C[ pc ] );
		}
	}
	// Edge rows [mb,M) over cols [0,nb):
	for ( jj = 0; jj < nb; jj++ ) {
		pb = ob + ( jj * bn );
		for ( ii = mb; ii < M; ii++ ) {
			temp = 0.0;
			pa = oa + ( ii * ar );
			for ( l = 0; l < K; l++ ) { temp += A[ pa + ( l*ak ) ] * B[ pb + ( l*bk ) ]; }
			pc = oc + ( ii*sc1 ) + ( jj*sc2 );
			C[ pc ] = ( beta === 0.0 ) ? alpha*temp : ( alpha*temp ) + ( beta*C[ pc ] );
		}
	}
	return C;
}

module.exports = dgemm;
