'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztrtrs = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_no_trans = require( './fixtures/upper_no_trans.json' );
var lower_no_trans = require( './fixtures/lower_no_trans.json' );
var upper_trans = require( './fixtures/upper_trans.json' );
var upper_conj_trans = require( './fixtures/upper_conj_trans.json' );
var upper_unit_diag = require( './fixtures/upper_unit_diag.json' );
var n_zero = require( './fixtures/n_zero.json' );
var singular = require( './fixtures/singular.json' );
var multi_rhs = require( './fixtures/multi_rhs.json' );
var lower_trans = require( './fixtures/lower_trans.json' );
var lower_conj_trans = require( './fixtures/lower_conj_trans.json' );
var singular_first = require( './fixtures/singular_first.json' );
var singular_last = require( './fixtures/singular_last.json' );
var nrhs_zero = require( './fixtures/nrhs_zero.json' );

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

test( 'ztrtrs: upper triangular, no transpose', function t() {
	var tc = upper_no_trans;
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  4, 1,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'upper', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: lower triangular, no transpose', function t() {
	var tc = lower_no_trans;
	var A = new Complex128Array([
		2, 1,  1, 2,  3, 0,
		0, 0,  4, 1,  5, 2,
		0, 0,  0, 0,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'lower', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: upper triangular, transpose', function t() {
	var tc = upper_trans;
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  4, 1,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'upper', 'transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: upper triangular, conjugate transpose', function t() {
	var tc = upper_conj_trans;
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  4, 1,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'upper', 'conjugate-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: upper triangular, unit diagonal', function t() {
	var tc = upper_unit_diag;
	var A = new Complex128Array([
		99, 99,  0, 0,  0, 0,
		2, 1,  99, 99,  0, 0,
		3, 0,  4, 2,  99, 99
	]);
	var B = new Complex128Array([
		10, 5,  5, 3,  1, 1
	]);
	var info = ztrtrs( 'upper', 'no-transpose', 'unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: N=0 quick return', function t() {
	var tc = n_zero;
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var info = ztrtrs( 'upper', 'no-transpose', 'non-unit', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'ztrtrs: singular diagonal (element 2)', function t() {
	var tc = singular;
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  0, 0,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'upper', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'ztrtrs: multiple right-hand sides (NRHS=2)', function t() {
	var tc = multi_rhs;
	var A = new Complex128Array([
		2, 1,  0, 0,  0, 0,
		1, 2,  4, 1,  0, 0,
		3, 0,  5, 2,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2,
		4, 1,  5, 2,  6, 3
	]);
	var info = ztrtrs( 'upper', 'no-transpose', 'non-unit', 3, 2, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: lower triangular, transpose', function t() {
	var tc = lower_trans;
	var A = new Complex128Array([
		2, 1,  1, 2,  3, 0,
		0, 0,  4, 1,  5, 2,
		0, 0,  0, 0,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'lower', 'transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: lower triangular, conjugate transpose', function t() {
	var tc = lower_conj_trans;
	var A = new Complex128Array([
		2, 1,  1, 2,  3, 0,
		0, 0,  4, 1,  5, 2,
		0, 0,  0, 0,  6, 1
	]);
	var B = new Complex128Array([
		1, 0,  2, 1,  3, 2
	]);
	var info = ztrtrs( 'lower', 'conjugate-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	var Bv = reinterpret( B, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.x, 1e-14, 'x' );
});

test( 'ztrtrs: singular at first diagonal element', function t() {
	var tc = singular_first;
	var A = new Complex128Array([
		0, 0,  0, 0,  0, 0,
		1, 0,  2, 0,  0, 0,
		1, 0,  1, 0,  3, 0
	]);
	var B = new Complex128Array([
		1, 0,  2, 0,  3, 0
	]);
	var info = ztrtrs( 'upper', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'ztrtrs: singular at last diagonal element', function t() {
	var tc = singular_last;
	var A = new Complex128Array([
		2, 0,  0, 0,  0, 0,
		1, 0,  3, 0,  0, 0,
		1, 0,  1, 0,  0, 0
	]);
	var B = new Complex128Array([
		1, 0,  2, 0,  3, 0
	]);
	var info = ztrtrs( 'upper', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'ztrtrs: NRHS=0 (no right-hand sides)', function t() {
	var tc = nrhs_zero;
	var A = new Complex128Array([ 2, 1 ]);
	var B = new Complex128Array( 1 );
	var info = ztrtrs( 'upper', 'no-transpose', 'non-unit', 1, 0, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var A = new Complex128Array( 9 );
	var B = new Complex128Array( 3 );
	assert.throws( function f() {
		ndarray( 'foo', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid trans', function t() {
	var A = new Complex128Array( 9 );
	var B = new Complex128Array( 3 );
	assert.throws( function f() {
		ndarray( 'upper', 'foo', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var A = new Complex128Array( 9 );
	var B = new Complex128Array( 3 );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'foo', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var A = new Complex128Array( 9 );
	var B = new Complex128Array( 3 );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', -1, 1, A, 1, 3, 0, B, 1, 3, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for negative NRHS', function t() {
	var A = new Complex128Array( 9 );
	var B = new Complex128Array( 3 );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', 3, -1, A, 1, 3, 0, B, 1, 3, 0 );
	}, RangeError );
});

test( 'ndarray: N=0 early return', function t() {
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var info = ndarray( 'upper', 'no-transpose', 'non-unit', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, 0 );
});
