'use strict';

/* eslint-disable max-len, max-params, max-statements */

// v2: Register blocking over N (4 columns of C at a time) for the contiguous
// column-major NN case. For each k, a single A[i,k] load is reused across 4
// columns of C, multiplied by 4 distinct B scalars -> ~4x fewer A reads and
// higher arithmetic intensity. Tail columns handled by the v1 unit-stride path.

var ref = require( './v0-reference.js' );

function scaleCol( C, pc, M, beta ) {
	var i;
	if ( beta === 0.0 ) {
		for ( i = 0; i < M; i++ ) {
			C[ pc + i ] = 0.0;
		}
	} else if ( beta !== 1.0 ) {
		for ( i = 0; i < M; i++ ) {
			C[ pc + i ] *= beta;
		}
	}
}

function dgemm( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc ) {
	var b0;
	var b1;
	var b2;
	var b3;
	var av;
	var pa;
	var pc0;
	var pc1;
	var pc2;
	var pc3;
	var pb0;
	var pb1;
	var pb2;
	var pb3;
	var temp;
	var pb;
	var pc;
	var j;
	var i;
	var l;

	if ( M === 0 || N === 0 || ( ( alpha === 0.0 || K === 0 ) && beta === 1.0 ) ) {
		return C;
	}
	if ( transa !== 'no-transpose' || transb !== 'no-transpose' || sa1 !== 1 || sc1 !== 1 || alpha === 0.0 ) {
		return ref( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc );
	}

	j = 0;
	for ( ; j + 4 <= N; j += 4 ) {
		pc0 = oc + (j * sc2);
		pc1 = pc0 + sc2;
		pc2 = pc1 + sc2;
		pc3 = pc2 + sc2;
		scaleCol( C, pc0, M, beta );
		scaleCol( C, pc1, M, beta );
		scaleCol( C, pc2, M, beta );
		scaleCol( C, pc3, M, beta );
		pb0 = ob + (j * sb2);
		pb1 = pb0 + sb2;
		pb2 = pb1 + sb2;
		pb3 = pb2 + sb2;
		for ( l = 0; l < K; l++ ) {
			b0 = alpha * B[ pb0 + (l * sb1) ];
			b1 = alpha * B[ pb1 + (l * sb1) ];
			b2 = alpha * B[ pb2 + (l * sb1) ];
			b3 = alpha * B[ pb3 + (l * sb1) ];
			pa = oa + (l * sa2);
			for ( i = 0; i < M; i++ ) {
				av = A[ pa + i ];
				C[ pc0 + i ] += b0 * av;
				C[ pc1 + i ] += b1 * av;
				C[ pc2 + i ] += b2 * av;
				C[ pc3 + i ] += b3 * av;
			}
		}
	}
	// Tail columns:
	for ( ; j < N; j++ ) {
		pc = oc + (j * sc2);
		scaleCol( C, pc, M, beta );
		pb = ob + (j * sb2);
		for ( l = 0; l < K; l++ ) {
			temp = alpha * B[ pb + (l * sb1) ];
			pa = oa + (l * sa2);
			for ( i = 0; i < M; i++ ) {
				C[ pc + i ] += temp * A[ pa + i ];
			}
		}
	}
	return C;
}

module.exports = dgemm;
