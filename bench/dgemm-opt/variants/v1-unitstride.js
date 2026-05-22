'use strict';

/* eslint-disable max-len, max-params, max-statements */

// v1: Specialize the hot no-transpose/no-transpose path for unit row-stride
// (contiguous column-major: strideA1 === strideC1 === 1). Folds the stride
// increments into the index, giving V8 a single induction variable per loop.
// Falls back to the reference for every other case.

var ref = require( './v0-reference.js' );

function dgemm( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc ) {
	var temp;
	var pa;
	var pc;
	var pb;
	var i;
	var j;
	var l;

	if ( M === 0 || N === 0 || ( ( alpha === 0.0 || K === 0 ) && beta === 1.0 ) ) {
		return C;
	}
	// Only the contiguous column-major NN case is specialized here:
	if ( transa !== 'no-transpose' || transb !== 'no-transpose' || sa1 !== 1 || sc1 !== 1 ) {
		return ref( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc );
	}
	if ( alpha === 0.0 ) {
		return ref( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc );
	}
	for ( j = 0; j < N; j++ ) {
		pc = oc + (j * sc2);
		if ( beta === 0.0 ) {
			for ( i = 0; i < M; i++ ) {
				C[ pc + i ] = 0.0;
			}
		} else if ( beta !== 1.0 ) {
			for ( i = 0; i < M; i++ ) {
				C[ pc + i ] *= beta;
			}
		}
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
