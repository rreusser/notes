'use strict';

/* eslint-disable max-len, max-params, max-statements */

// v7: One-level Strassen for square, even-dimension, contiguous column-major NN.
// Splits into 2x2 blocks and uses 7 (instead of 8) sub-multiplies, each run via
// the v5 cache-blocked kernel; everything else falls back to v5. This is a
// proof/benchmark variant: Strassen trades multiplies for extra matrix
// additions + temporary allocations, so it is expected to LOSE in the
// bandwidth-bound large regime. Included to measure that claim, not to ship.

var kernel = require( './v5-blocked4x4.js' );

// out = A_block (op) B_block, simple (n x n) contiguous col-major helpers:
function addsub( dst, a, oa, b, ob, h, lead, sign ) {
	// dst (h x h, contiguous, ld=h) = a_block +/- b_block, both ld=lead.
	var j, i, pd, pa, pb;
	for ( j = 0; j < h; j++ ) {
		pd = j * h;
		pa = oa + ( j * lead );
		pb = ob + ( j * lead );
		for ( i = 0; i < h; i++ ) {
			dst[ pd + i ] = a[ pa + i ] + ( sign * b[ pb + i ] );
		}
	}
}

function dgemm( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc ) {
	var h, n, lead;
	var t1, t2, m1, m2, m3, m4, m5, m6, m7;
	var oA11, oA12, oA21, oA22, oB11, oB12, oB21, oB22;
	var i, j, pc, pm;

	// Only square, even, contiguous col-major NN with alpha=1 is specialized:
	if (
		transa !== 'no-transpose' || transb !== 'no-transpose' ||
		sa1 !== 1 || sb1 !== 1 || sc1 !== 1 ||
		M !== N || N !== K || ( M % 2 ) !== 0 || M < 64 ||
		sa2 !== M || sb2 !== M || sc2 !== M || oa !== 0 || ob !== 0 || oc !== 0
	) {
		return kernel( transa, transb, M, N, K, alpha, A, sa1, sa2, oa, B, sb1, sb2, ob, beta, C, sc1, sc2, oc );
	}
	n = M;
	h = n / 2;
	lead = n;

	// Block offsets (col-major, leading dim n):
	oA11 = 0;       oA21 = h;       oA12 = h*n;       oA22 = h*n + h;
	oB11 = 0;       oB21 = h;       oB12 = h*n;       oB22 = h*n + h;

	t1 = new Float64Array( h*h );
	t2 = new Float64Array( h*h );
	m1 = new Float64Array( h*h );
	m2 = new Float64Array( h*h );
	m3 = new Float64Array( h*h );
	m4 = new Float64Array( h*h );
	m5 = new Float64Array( h*h );
	m6 = new Float64Array( h*h );
	m7 = new Float64Array( h*h );

	function mul( dst, a, b ) {
		kernel( 'no-transpose', 'no-transpose', h, h, h, 1.0, a, 1, h, 0, b, 1, h, 0, 0.0, dst, 1, h, 0 );
	}

	// M1 = (A11+A22)(B11+B22)
	addsub( t1, A, oA11, A, oA22, h, lead, 1 );
	addsub( t2, B, oB11, B, oB22, h, lead, 1 );
	mul( m1, t1, t2 );
	// M2 = (A21+A22) B11
	addsub( t1, A, oA21, A, oA22, h, lead, 1 );
	addsub( t2, B, oB11, B, oB11, h, lead, 0 ); // copy B11
	mul( m2, t1, t2 );
	// M3 = A11 (B12-B22)
	addsub( t1, A, oA11, A, oA11, h, lead, 0 );
	addsub( t2, B, oB12, B, oB22, h, lead, -1 );
	mul( m3, t1, t2 );
	// M4 = A22 (B21-B11)
	addsub( t1, A, oA22, A, oA22, h, lead, 0 );
	addsub( t2, B, oB21, B, oB11, h, lead, -1 );
	mul( m4, t1, t2 );
	// M5 = (A11+A12) B22
	addsub( t1, A, oA11, A, oA12, h, lead, 1 );
	addsub( t2, B, oB22, B, oB22, h, lead, 0 );
	mul( m5, t1, t2 );
	// M6 = (A21-A11)(B11+B12)
	addsub( t1, A, oA21, A, oA11, h, lead, -1 );
	addsub( t2, B, oB11, B, oB12, h, lead, 1 );
	mul( m6, t1, t2 );
	// M7 = (A12-A22)(B21+B22)
	addsub( t1, A, oA12, A, oA22, h, lead, -1 );
	addsub( t2, B, oB21, B, oB22, h, lead, 1 );
	mul( m7, t1, t2 );

	// Combine into C with alpha/beta. P blocks:
	//   C11 = M1+M4-M5+M7 ; C12 = M3+M5 ; C21 = M2+M4 ; C22 = M1-M2+M3+M6
	for ( j = 0; j < h; j++ ) {
		pm = j * h;
		// C11
		pc = oc + ( j * n );
		for ( i = 0; i < h; i++ ) {
			C[ pc + i ] = ( alpha * ( m1[pm+i] + m4[pm+i] - m5[pm+i] + m7[pm+i] ) ) + ( beta === 0.0 ? 0.0 : beta * C[ pc + i ] );
		}
		// C21
		pc = oc + h + ( j * n );
		for ( i = 0; i < h; i++ ) {
			C[ pc + i ] = ( alpha * ( m2[pm+i] + m4[pm+i] ) ) + ( beta === 0.0 ? 0.0 : beta * C[ pc + i ] );
		}
		// C12
		pc = oc + ( ( j+h ) * n );
		for ( i = 0; i < h; i++ ) {
			C[ pc + i ] = ( alpha * ( m3[pm+i] + m5[pm+i] ) ) + ( beta === 0.0 ? 0.0 : beta * C[ pc + i ] );
		}
		// C22
		pc = oc + h + ( ( j+h ) * n );
		for ( i = 0; i < h; i++ ) {
			C[ pc + i ] = ( alpha * ( m1[pm+i] - m2[pm+i] + m3[pm+i] + m6[pm+i] ) ) + ( beta === 0.0 ? 0.0 : beta * C[ pc + i ] );
		}
	}
	return C;
}

module.exports = dgemm;
