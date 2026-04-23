'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zpbequ = require( './../lib/base.js' );

// FIXTURES //

var upper_basic = require( './fixtures/upper_basic.json' );
var lower_basic = require( './fixtures/lower_basic.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var non_positive_upper = require( './fixtures/non_positive_upper.json' );
var zero_diag_lower = require( './fixtures/zero_diag_lower.json' );
var identity_upper = require( './fixtures/identity_upper.json' );
var diagonal_varied_lower = require( './fixtures/diagonal_varied_lower.json' );
var non_positive_first = require( './fixtures/non_positive_first.json' );
var non_positive_last = require( './fixtures/non_positive_last.json' );

// FUNCTIONS //

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

// TESTS //

test( 'zpbequ is a function', function t() {
	assert.equal( typeof zpbequ, 'function' );
});

test( 'zpbequ: upper_basic', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = upper_basic;

	// UPLO='U', N=4, KD=2, LDAB=3 (column-major band storage)
	// Diagonal at row KD=2 (0-based), diags real parts: 4, 9, 16, 25
	// Each column has 3 complex elements (LDAB=3)
	// Interleaved re/im: col1=[row0_re,row0_im, row1_re,row1_im, row2_re,row2_im], ...
	AB = new Complex128Array([
		0.0, 0.0,   0.0, 0.0,   4.0, 0.0,    // col 1
		0.0, 0.0,   1.0, 0.5,   9.0, 0.0,    // col 2
		0.5, 0.3,   2.0, 1.0,   16.0, 0.0,   // col 3
		0.0, 0.0,   1.5, 0.2,   25.0, 0.0    // col 4
	]);
	s = new Float64Array( 4 );

	// strideAB1=1 (row), strideAB2=3 (LDAB=3), offsetAB=0
	result = zpbequ( 'upper', 4, 2, AB, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'zpbequ: lower_basic', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = lower_basic;

	// UPLO='L', N=4, KD=2, LDAB=3 (column-major band storage)
	// Diagonal at row 0 (0-based), diags real parts: 4, 9, 16, 25
	AB = new Complex128Array([
		4.0, 0.0,    1.0, 0.5,    0.5, 0.3,    // col 1
		9.0, 0.0,    2.0, 1.0,    0.0, 0.0,    // col 2
		16.0, 0.0,   1.5, 0.2,    0.0, 0.0,    // col 3
		25.0, 0.0,   0.0, 0.0,    0.0, 0.0     // col 4
	]);
	s = new Float64Array( 4 );

	result = zpbequ( 'lower', 4, 2, AB, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'zpbequ: n_zero', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = n_zero;
	AB = new Complex128Array( 1 );
	s = new Float64Array( 1 );

	result = zpbequ( 'upper', 0, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zpbequ: n_one', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = n_one;

	// N=1, KD=0, LDAB=1
	AB = new Complex128Array([ 49.0, 0.0 ]);
	s = new Float64Array( 1 );

	result = zpbequ( 'upper', 1, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'zpbequ: non_positive_upper', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = non_positive_upper;

	// UPLO='U', N=3, KD=1, LDAB=2
	// Diagonal at row KD=1 (0-based): real parts 4, -1, 9
	AB = new Complex128Array([
		0.0, 0.0,    4.0, 0.0,      // col 1
		1.0, 0.5,    -1.0, 0.0,     // col 2
		0.5, 0.3,    9.0, 0.0       // col 3
	]);
	s = new Float64Array( 3 );

	result = zpbequ( 'upper', 3, 1, AB, 1, 2, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'zpbequ: zero_diag_lower', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = zero_diag_lower;

	// UPLO='L', N=3, KD=1, LDAB=2
	// Diagonal at row 0 (0-based): real parts 4, 0, 9
	AB = new Complex128Array([
		4.0, 0.0,    1.0, 0.5,      // col 1
		0.0, 0.0,    0.5, 0.3,      // col 2
		9.0, 0.0,    0.0, 0.0       // col 3
	]);
	s = new Float64Array( 3 );

	result = zpbequ( 'lower', 3, 1, AB, 1, 2, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'zpbequ: identity_upper', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = identity_upper;

	// UPLO='U', N=3, KD=1, LDAB=2
	// Diagonal at row 1 (0-based): all 1s
	AB = new Complex128Array([
		0.0, 0.0,   1.0, 0.0,    // col 1
		0.0, 0.0,   1.0, 0.0,    // col 2
		0.0, 0.0,   1.0, 0.0     // col 3
	]);
	s = new Float64Array( 3 );

	result = zpbequ( 'upper', 3, 1, AB, 1, 2, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'zpbequ: diagonal_varied_lower', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = diagonal_varied_lower;

	// UPLO='L', N=3, KD=0, LDAB=1
	// Diagonal at row 0: 100, 1, 0.25
	AB = new Complex128Array([ 100.0, 0.0, 1.0, 0.0, 0.25, 0.0 ]);
	s = new Float64Array( 3 );

	result = zpbequ( 'lower', 3, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'zpbequ: non_positive_first', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = non_positive_first;

	// UPLO='L', N=3, KD=0, LDAB=1
	// Diagonal real parts: -2, 4, 9
	AB = new Complex128Array([ -2.0, 0.0, 4.0, 0.0, 9.0, 0.0 ]);
	s = new Float64Array( 3 );

	result = zpbequ( 'lower', 3, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'zpbequ: non_positive_last', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = non_positive_last;

	// UPLO='U', N=3, KD=0, LDAB=1
	// Diagonal real parts: 4, 9, -3
	AB = new Complex128Array([ 4.0, 0.0, 9.0, 0.0, -3.0, 0.0 ]);
	s = new Float64Array( 3 );

	result = zpbequ( 'upper', 3, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});
