

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlatbs = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_notrans_nonunit_4x4 = require( './fixtures/upper_notrans_nonunit_4x4.json' );
var lower_notrans_nonunit_4x4 = require( './fixtures/lower_notrans_nonunit_4x4.json' );
var upper_conjtrans_nonunit_4x4 = require( './fixtures/upper_conjtrans_nonunit_4x4.json' );
var upper_trans_nonunit_4x4 = require( './fixtures/upper_trans_nonunit_4x4.json' );
var lower_notrans_unit_4x4 = require( './fixtures/lower_notrans_unit_4x4.json' );
var n0 = require( './fixtures/n0.json' );
var n1_upper = require( './fixtures/n1_upper.json' );
var lower_conjtrans_nonunit_4x4 = require( './fixtures/lower_conjtrans_nonunit_4x4.json' );
var singular_upper = require( './fixtures/singular_upper.json' );

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

// TESTS //

test( 'zlatbs: upper_notrans_nonunit_4x4', function t() {
	var tc = upper_notrans_nonunit_4x4;
	var n = 4;
	var kd = 2;
	// Band storage: (kd+1) x n, column-major
	var AB = new Complex128Array( (kd + 1) * n );
	var ABv = reinterpret( AB, 0 );
	var ldab = kd + 1;
	// Diagonal: AB(kd, j) = A(j,j)
	ABv[ (2 + 0 * ldab) * 2 ] = 2.0; ABv[ (2 + 0 * ldab) * 2 + 1 ] = 1.0;
	ABv[ (2 + 1 * ldab) * 2 ] = 3.0; ABv[ (2 + 1 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (2 + 2 * ldab) * 2 ] = 4.0; ABv[ (2 + 2 * ldab) * 2 + 1 ] = 2.0;
	ABv[ (2 + 3 * ldab) * 2 ] = 5.0; ABv[ (2 + 3 * ldab) * 2 + 1 ] = 0.0;
	// Superdiag 1: AB(1, j)
	ABv[ (1 + 1 * ldab) * 2 ] = 1.0; ABv[ (1 + 1 * ldab) * 2 + 1 ] = 0.5;
	ABv[ (1 + 2 * ldab) * 2 ] = 2.0; ABv[ (1 + 2 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (1 + 3 * ldab) * 2 ] = 1.5; ABv[ (1 + 3 * ldab) * 2 + 1 ] = 1.0;
	// Superdiag 2: AB(0, j)
	ABv[ (0 + 2 * ldab) * 2 ] = 0.5; ABv[ (0 + 2 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (0 + 3 * ldab) * 2 ] = 0.3; ABv[ (0 + 3 * ldab) * 2 + 1 ] = 0.2;

	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 1.0, -1.0, 3.0, 0.5, -0.5 ] );
	var scale = new Float64Array( 1 );
	var CNORM = new Float64Array( n );
	var info = zlatbs( 'upper', 'no-transpose', 'non-unit', 'no', n, kd, AB, 1, ldab, 0, x, 1, 0, scale, CNORM, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( CNORM ), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatbs: lower_notrans_nonunit_4x4', function t() {
	var tc = lower_notrans_nonunit_4x4;
	var n = 4;
	var kd = 2;
	var AB = new Complex128Array( (kd + 1) * n );
	var ABv = reinterpret( AB, 0 );
	var ldab = kd + 1;
	// Lower band storage: AB(0, j) = A(j,j), AB(1, j) = A(j+1,j), AB(2, j) = A(j+2,j)
	ABv[ (0 + 0 * ldab) * 2 ] = 3.0; ABv[ (0 + 0 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (0 + 1 * ldab) * 2 ] = 4.0; ABv[ (0 + 1 * ldab) * 2 + 1 ] = 1.0;
	ABv[ (0 + 2 * ldab) * 2 ] = 2.0; ABv[ (0 + 2 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (0 + 3 * ldab) * 2 ] = 5.0; ABv[ (0 + 3 * ldab) * 2 + 1 ] = 2.0;
	ABv[ (1 + 0 * ldab) * 2 ] = 1.0; ABv[ (1 + 0 * ldab) * 2 + 1 ] = 1.0;
	ABv[ (1 + 1 * ldab) * 2 ] = 0.5; ABv[ (1 + 1 * ldab) * 2 + 1 ] = -0.5;
	ABv[ (1 + 2 * ldab) * 2 ] = 2.0; ABv[ (1 + 2 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (2 + 0 * ldab) * 2 ] = 0.2; ABv[ (2 + 0 * ldab) * 2 + 1 ] = 0.1;
	ABv[ (2 + 1 * ldab) * 2 ] = 0.3; ABv[ (2 + 1 * ldab) * 2 + 1 ] = -0.2;

	var x = new Complex128Array( [ 1.0, 0.0, 0.0, 1.0, 2.0, -1.0, -1.0, 0.5 ] );
	var scale = new Float64Array( 1 );
	var CNORM = new Float64Array( n );
	var info = zlatbs( 'lower', 'no-transpose', 'non-unit', 'no', n, kd, AB, 1, ldab, 0, x, 1, 0, scale, CNORM, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( CNORM ), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatbs: upper_conjtrans_nonunit_4x4', function t() {
	var tc = upper_conjtrans_nonunit_4x4;
	var n = 4;
	var kd = 2;
	var AB = new Complex128Array( (kd + 1) * n );
	var ABv = reinterpret( AB, 0 );
	var ldab = kd + 1;
	ABv[ (2 + 0 * ldab) * 2 ] = 2.0; ABv[ (2 + 0 * ldab) * 2 + 1 ] = 1.0;
	ABv[ (2 + 1 * ldab) * 2 ] = 3.0; ABv[ (2 + 1 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (2 + 2 * ldab) * 2 ] = 4.0; ABv[ (2 + 2 * ldab) * 2 + 1 ] = 2.0;
	ABv[ (2 + 3 * ldab) * 2 ] = 5.0; ABv[ (2 + 3 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (1 + 1 * ldab) * 2 ] = 1.0; ABv[ (1 + 1 * ldab) * 2 + 1 ] = 0.5;
	ABv[ (1 + 2 * ldab) * 2 ] = 2.0; ABv[ (1 + 2 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (1 + 3 * ldab) * 2 ] = 1.5; ABv[ (1 + 3 * ldab) * 2 + 1 ] = 1.0;
	ABv[ (0 + 2 * ldab) * 2 ] = 0.5; ABv[ (0 + 2 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (0 + 3 * ldab) * 2 ] = 0.3; ABv[ (0 + 3 * ldab) * 2 + 1 ] = 0.2;

	var x = new Complex128Array( [ 1.0, 2.0, -1.0, 1.0, 0.5, -0.5, 3.0, 0.0 ] );
	var scale = new Float64Array( 1 );
	var CNORM = new Float64Array( n );
	var info = zlatbs( 'upper', 'conjugate-transpose', 'non-unit', 'no', n, kd, AB, 1, ldab, 0, x, 1, 0, scale, CNORM, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( CNORM ), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatbs: upper_trans_nonunit_4x4', function t() {
	var tc = upper_trans_nonunit_4x4;
	var n = 4;
	var kd = 2;
	var AB = new Complex128Array( (kd + 1) * n );
	var ABv = reinterpret( AB, 0 );
	var ldab = kd + 1;
	ABv[ (2 + 0 * ldab) * 2 ] = 2.0; ABv[ (2 + 0 * ldab) * 2 + 1 ] = 1.0;
	ABv[ (2 + 1 * ldab) * 2 ] = 3.0; ABv[ (2 + 1 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (2 + 2 * ldab) * 2 ] = 4.0; ABv[ (2 + 2 * ldab) * 2 + 1 ] = 2.0;
	ABv[ (2 + 3 * ldab) * 2 ] = 5.0; ABv[ (2 + 3 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (1 + 1 * ldab) * 2 ] = 1.0; ABv[ (1 + 1 * ldab) * 2 + 1 ] = 0.5;
	ABv[ (1 + 2 * ldab) * 2 ] = 2.0; ABv[ (1 + 2 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (1 + 3 * ldab) * 2 ] = 1.5; ABv[ (1 + 3 * ldab) * 2 + 1 ] = 1.0;
	ABv[ (0 + 2 * ldab) * 2 ] = 0.5; ABv[ (0 + 2 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (0 + 3 * ldab) * 2 ] = 0.3; ABv[ (0 + 3 * ldab) * 2 + 1 ] = 0.2;

	var x = new Complex128Array( [ 1.0, 2.0, -1.0, 1.0, 0.5, -0.5, 3.0, 0.0 ] );
	var scale = new Float64Array( 1 );
	var CNORM = new Float64Array( n );
	var info = zlatbs( 'upper', 'transpose', 'non-unit', 'no', n, kd, AB, 1, ldab, 0, x, 1, 0, scale, CNORM, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( CNORM ), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatbs: lower_notrans_unit_4x4', function t() {
	var tc = lower_notrans_unit_4x4;
	var n = 4;
	var kd = 2;
	var AB = new Complex128Array( (kd + 1) * n );
	var ABv = reinterpret( AB, 0 );
	var ldab = kd + 1;
	// Diagonal (ignored for unit, but set anyway)
	ABv[ (0 + 0 * ldab) * 2 ] = 999.0; ABv[ (0 + 0 * ldab) * 2 + 1 ] = 999.0;
	ABv[ (0 + 1 * ldab) * 2 ] = 999.0; ABv[ (0 + 1 * ldab) * 2 + 1 ] = 999.0;
	ABv[ (0 + 2 * ldab) * 2 ] = 999.0; ABv[ (0 + 2 * ldab) * 2 + 1 ] = 999.0;
	ABv[ (0 + 3 * ldab) * 2 ] = 999.0; ABv[ (0 + 3 * ldab) * 2 + 1 ] = 999.0;
	// Subdiag 1
	ABv[ (1 + 0 * ldab) * 2 ] = 0.5; ABv[ (1 + 0 * ldab) * 2 + 1 ] = 0.5;
	ABv[ (1 + 1 * ldab) * 2 ] = 1.0; ABv[ (1 + 1 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (1 + 2 * ldab) * 2 ] = -0.5; ABv[ (1 + 2 * ldab) * 2 + 1 ] = 0.25;
	// Subdiag 2: all zero (default)

	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 1.0, -1.0, 2.0, 0.5, -0.5 ] );
	var scale = new Float64Array( 1 );
	var CNORM = new Float64Array( n );
	var info = zlatbs( 'lower', 'no-transpose', 'unit', 'no', n, kd, AB, 1, ldab, 0, x, 1, 0, scale, CNORM, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( CNORM ), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatbs: n0', function t() {
	var tc = n0;
	var AB = new Complex128Array( 1 );
	var x = new Complex128Array( 1 );
	var scale = new Float64Array( 1 );
	var CNORM = new Float64Array( 1 );
	var info = zlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 0, 0, AB, 1, 1, 0, x, 1, 0, scale, CNORM, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
});

test( 'zlatbs: n1_upper', function t() {
	var tc = n1_upper;
	var n = 1;
	var kd = 2;
	var AB = new Complex128Array( 3 );
	var ABv = reinterpret( AB, 0 );
	var ldab = kd + 1;
	// Diagonal at row kd=2
	ABv[ (2 + 0 * ldab) * 2 ] = 3.0; ABv[ (2 + 0 * ldab) * 2 + 1 ] = 4.0;
	var x = new Complex128Array( [ 5.0, -2.0 ] );
	var scale = new Float64Array( 1 );
	var CNORM = new Float64Array( 1 );
	var info = zlatbs( 'upper', 'no-transpose', 'non-unit', 'no', n, kd, AB, 1, ldab, 0, x, 1, 0, scale, CNORM, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.X, 1e-14, 'X' );
});

test( 'zlatbs: lower_conjtrans_nonunit_4x4', function t() {
	var tc = lower_conjtrans_nonunit_4x4;
	var n = 4;
	var kd = 2;
	var AB = new Complex128Array( (kd + 1) * n );
	var ABv = reinterpret( AB, 0 );
	var ldab = kd + 1;
	ABv[ (0 + 0 * ldab) * 2 ] = 3.0; ABv[ (0 + 0 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (0 + 1 * ldab) * 2 ] = 4.0; ABv[ (0 + 1 * ldab) * 2 + 1 ] = 1.0;
	ABv[ (0 + 2 * ldab) * 2 ] = 2.0; ABv[ (0 + 2 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (0 + 3 * ldab) * 2 ] = 5.0; ABv[ (0 + 3 * ldab) * 2 + 1 ] = 2.0;
	ABv[ (1 + 0 * ldab) * 2 ] = 1.0; ABv[ (1 + 0 * ldab) * 2 + 1 ] = 1.0;
	ABv[ (1 + 1 * ldab) * 2 ] = 0.5; ABv[ (1 + 1 * ldab) * 2 + 1 ] = -0.5;
	ABv[ (1 + 2 * ldab) * 2 ] = 2.0; ABv[ (1 + 2 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (2 + 0 * ldab) * 2 ] = 0.2; ABv[ (2 + 0 * ldab) * 2 + 1 ] = 0.1;
	ABv[ (2 + 1 * ldab) * 2 ] = 0.3; ABv[ (2 + 1 * ldab) * 2 + 1 ] = -0.2;

	var x = new Complex128Array( [ 1.0, -1.0, 2.0, 0.0, 0.0, 3.0, -1.0, 1.0 ] );
	var scale = new Float64Array( 1 );
	var CNORM = new Float64Array( n );
	var info = zlatbs( 'lower', 'conjugate-transpose', 'non-unit', 'no', n, kd, AB, 1, ldab, 0, x, 1, 0, scale, CNORM, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( CNORM ), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatbs: singular_upper', function t() {
	var tc = singular_upper;
	var n = 3;
	var kd = 2;
	var AB = new Complex128Array( (kd + 1) * n );
	var ABv = reinterpret( AB, 0 );
	var ldab = kd + 1;
	// Diagonal at row kd=2
	ABv[ (2 + 0 * ldab) * 2 ] = 2.0; ABv[ (2 + 0 * ldab) * 2 + 1 ] = 0.0;
	ABv[ (2 + 1 * ldab) * 2 ] = 0.0; ABv[ (2 + 1 * ldab) * 2 + 1 ] = 0.0; // singular!
	ABv[ (2 + 2 * ldab) * 2 ] = 3.0; ABv[ (2 + 2 * ldab) * 2 + 1 ] = 1.0;
	// Superdiag 1 at row 1
	ABv[ (1 + 1 * ldab) * 2 ] = 1.0; ABv[ (1 + 1 * ldab) * 2 + 1 ] = -1.0;
	ABv[ (1 + 2 * ldab) * 2 ] = 0.5; ABv[ (1 + 2 * ldab) * 2 + 1 ] = 0.5;

	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 1.0, -1.0, 0.0 ] );
	var scale = new Float64Array( 1 );
	var CNORM = new Float64Array( n );
	var info = zlatbs( 'upper', 'no-transpose', 'non-unit', 'no', n, kd, AB, 1, ldab, 0, x, 1, 0, scale, CNORM, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.X, 1e-14, 'X' );
});
