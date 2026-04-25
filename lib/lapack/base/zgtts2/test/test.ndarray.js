'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgttrf = require( './../../zgttrf/lib/base.js' );
var zgtts2 = require( './../lib/ndarray.js' );

// FIXTURES //

var notrans_single_rhs = require( './fixtures/notrans_single_rhs.json' );
var trans_single_rhs = require( './fixtures/trans_single_rhs.json' );
var conjtrans_single_rhs = require( './fixtures/conjtrans_single_rhs.json' );
var notrans_multi_rhs = require( './fixtures/notrans_multi_rhs.json' );
var n_one = require( './fixtures/n_one.json' );
var n_two_notrans = require( './fixtures/n_two_notrans.json' );
var pivot_notrans = require( './fixtures/pivot_notrans.json' );
var pivot_conjtrans = require( './fixtures/pivot_conjtrans.json' );

// FUNCTIONS //

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Factorizes and solves: sets up complex tridiag arrays, calls zgttrf, then zgtts2.
* Returns the packed solution B as Float64Array (interleaved re/im).
*/
function solveTridiag( itrans, n, nrhs, dlArr, dArr, duArr, bArr ) {
	var dl = new Complex128Array( dlArr.length / 2 );
	var d = new Complex128Array( dArr.length / 2 );
	var du = new Complex128Array( duArr.length / 2 );
	var du2 = new Complex128Array( Math.max( n - 2, 0 ) );
	var B = new Complex128Array( n * nrhs );
	var ipiv = new Int32Array( n );
	var dlv = reinterpret( dl, 0 );
	var dv = reinterpret( d, 0 );
	var duv = reinterpret( du, 0 );
	var bv = reinterpret( B, 0 );
	var info;
	var i;

	for ( i = 0; i < dlArr.length; i++ ) {
		dlv[ i ] = dlArr[ i ];
	}
	for ( i = 0; i < dArr.length; i++ ) {
		dv[ i ] = dArr[ i ];
	}
	for ( i = 0; i < duArr.length; i++ ) {
		duv[ i ] = duArr[ i ];
	}
	for ( i = 0; i < bArr.length; i++ ) {
		bv[ i ] = bArr[ i ];
	}

	info = zgttrf( n, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'zgttrf info' );

	// B strides in Float64: strideB1 = 2 (one complex element), strideB2 = 2*n
	zgtts2( itrans, n, nrhs, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0, B, 2, 2 * n, 0 );

	return bv;
}

// TESTS //

test( 'zgtts2: no-transpose, single RHS (5x5)', function t() {
	var tc = notrans_single_rhs;
	var result = solveTridiag( 0, 5, 1,
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 2, 1, 2, 1, 2, 1, 2, 1, 2, 1 ],
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ]
	);
	assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'zgtts2: transpose (itrans=1), single RHS (5x5)', function t() {
	var tc = trans_single_rhs;
	var result = solveTridiag( 1, 5, 1,
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 2, 1, 2, 1, 2, 1, 2, 1, 2, 1 ],
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ]
	);
	assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'zgtts2: conjugate transpose (itrans=2), single RHS (5x5)', function t() {
	var tc = conjtrans_single_rhs;
	var result = solveTridiag( 2, 5, 1,
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 2, 1, 2, 1, 2, 1, 2, 1, 2, 1 ],
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ]
	);
	assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'zgtts2: no-transpose, multiple RHS (5x5, 2 columns)', function t() {
	var tc = notrans_multi_rhs;
	var result = solveTridiag( 0, 5, 2,
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 2, 1, 2, 1, 2, 1, 2, 1, 2, 1 ],
		[ -1, 0, -1, 0, -1, 0, -1, 0 ],
		[ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
	);
	assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'zgtts2: N=1', function t() {
	var tc = n_one;
	var result = solveTridiag( 0, 1, 1,
		[],
		[ 5, 2 ],
		[],
		[ 10, 4 ]
	);
	assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'zgtts2: N=2, no-transpose', function t() {
	var tc = n_two_notrans;
	var result = solveTridiag( 0, 2, 1,
		[ 3, 1 ],
		[ 4, 1, 7, 2 ],
		[ 1, 0.5 ],
		[ 5, 1, 10, 3 ]
	);
	assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'zgtts2: pivoting forced, no-transpose (5x5)', function t() {
	var tc = pivot_notrans;
	var result = solveTridiag( 0, 5, 1,
		[ 10, 1, 10, 1, 10, 1, 10, 1 ],
		[ 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ],
		[ 2, 0.5, 2, 0.5, 2, 0.5, 2, 0.5 ],
		[ 3, 1, 13, 2, 13, 2, 13, 2, 11, 1 ]
	);
	assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'zgtts2: pivoting forced, conjugate transpose (5x5)', function t() {
	var tc = pivot_conjtrans;
	var result = solveTridiag( 2, 5, 1,
		[ 10, 1, 10, 1, 10, 1, 10, 1 ],
		[ 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ],
		[ 2, 0.5, 2, 0.5, 2, 0.5, 2, 0.5 ],
		[ 3, 1, 13, 2, 13, 2, 13, 2, 11, 1 ]
	);
	assertArrayClose( result, tc.B, 1e-14, 'B' );
});

test( 'zgtts2: N=0, quick return', function t() {
	var dl = new Complex128Array( 0 );
	var d = new Complex128Array( 0 );
	var du = new Complex128Array( 0 );
	var du2 = new Complex128Array( 0 );
	var B = new Complex128Array( 0 );
	var ipiv = new Int32Array( 0 );
	// Should not throw
	zgtts2( 0, 0, 1, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0, B, 2, 0, 0 );
});
