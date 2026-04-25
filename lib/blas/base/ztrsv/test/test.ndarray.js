/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztrsv = require( './../lib/ndarray.js' );
var ndarray = require( './../lib/ndarray.js' );

// FIXTURES //

var ztrsv_upper_no_trans = require( './fixtures/ztrsv_upper_no_trans.json' );
var ztrsv_lower_no_trans = require( './fixtures/ztrsv_lower_no_trans.json' );
var ztrsv_unit_diag = require( './fixtures/ztrsv_unit_diag.json' );
var ztrsv_upper_trans = require( './fixtures/ztrsv_upper_trans.json' );
var ztrsv_upper_conjtrans = require( './fixtures/ztrsv_upper_conjtrans.json' );
var ztrsv_n_zero = require( './fixtures/ztrsv_n_zero.json' );
var ztrsv_n_one = require( './fixtures/ztrsv_n_one.json' );
var ztrsv_stride = require( './fixtures/ztrsv_stride.json' );
var ztrsv_lower_conjtrans = require( './fixtures/ztrsv_lower_conjtrans.json' );
var ztrsv_lower_trans = require( './fixtures/ztrsv_lower_trans.json' );
var ztrsv_neg_stride = require( './fixtures/ztrsv_neg_stride.json' );
var ztrsv_lower_unit = require( './fixtures/ztrsv_lower_unit.json' );
var ztrsv_upper_conjtrans_smith = require( './fixtures/ztrsv_upper_conjtrans_smith.json' );
var ztrsv_lower_conjtrans_smith = require( './fixtures/ztrsv_lower_conjtrans_smith.json' );

// FUNCTIONS //

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

// TESTS //

test( 'ztrsv: upper, no transpose, non-unit diagonal, N=2', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_upper_no_trans;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 2;
	Av[ 1 ] = 1;
	Av[ 8 ] = 3;
	Av[ 9 ] = 1;
	Av[ 10 ] = 4;
	Av[ 11 ] = 2;
	x = new Complex128Array( [ 7, 6, 2, 6 ] );
	ztrsv( 'upper', 'no-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, no transpose, non-unit diagonal, N=2', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_lower_no_trans;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 2;
	Av[ 1 ] = 1;
	Av[ 2 ] = 3;
	Av[ 3 ] = 1;
	Av[ 10 ] = 4;
	Av[ 11 ] = 2;
	x = new Complex128Array( [ 2, 1, 5, 7 ] );
	ztrsv( 'lower', 'no-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: upper, unit diagonal, N=2', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_unit_diag;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 99;
	Av[ 1 ] = 99;
	Av[ 8 ] = 3;
	Av[ 9 ] = 1;
	Av[ 10 ] = 99;
	Av[ 11 ] = 99;
	x = new Complex128Array( [ 3, 4, 1, 1 ] );
	ztrsv( 'upper', 'no-transpose', 'unit', 2, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: upper, transpose (no conj), non-unit, N=2', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_upper_trans;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 2;
	Av[ 1 ] = 1;
	Av[ 8 ] = 3;
	Av[ 9 ] = 1;
	Av[ 10 ] = 4;
	Av[ 11 ] = 2;
	x = new Complex128Array( [ 2, 1, 5, 7 ] );
	ztrsv( 'upper', 'transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: upper, conjugate transpose, non-unit, N=2', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_upper_conjtrans;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 2;
	Av[ 1 ] = 1;
	Av[ 8 ] = 3;
	Av[ 9 ] = 1;
	Av[ 10 ] = 4;
	Av[ 11 ] = 2;
	x = new Complex128Array( [ 2, -1, 9, 1 ] );
	ztrsv( 'upper', 'conjugate-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: N=0 quick return', function t() {
	var tc;
	var xv;
	var A;
	var x;

	tc = ztrsv_n_zero;
	A = new Complex128Array( 4 * 4 );
	x = new Complex128Array( [ 5, 5 ] );
	ztrsv( 'upper', 'no-transpose', 'non-unit', 0, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: N=1, upper, non-unit', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_n_one;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 3;
	Av[ 1 ] = 2;
	x = new Complex128Array( [ 4, 7 ] );
	ztrsv( 'upper', 'no-transpose', 'non-unit', 1, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: non-unit stride incx=2, upper, no transpose, N=2', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_stride;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 2;
	Av[ 1 ] = 0;
	Av[ 8 ] = 1;
	Av[ 9 ] = 1;
	Av[ 10 ] = 3;
	Av[ 11 ] = 0;
	x = new Complex128Array( [ 3, 1, 99, 99, 0, 3 ] );
	ztrsv( 'upper', 'no-transpose', 'non-unit', 2, A, 1, 4, 0, x, 2, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, conj-trans, non-unit, N=3', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_lower_conjtrans;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 1;
	Av[ 1 ] = 1;
	Av[ 2 ] = 2;
	Av[ 3 ] = 1;
	Av[ 4 ] = 3;
	Av[ 5 ] = 1;
	Av[ 10 ] = 4;
	Av[ 11 ] = 2;
	Av[ 12 ] = 5;
	Av[ 13 ] = 2;
	Av[ 20 ] = 6;
	Av[ 21 ] = 3;
	x = new Complex128Array( [ 1, 0, 2, 3, 4, 5 ] );
	ztrsv( 'lower', 'conjugate-transpose', 'non-unit', 3, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, transpose (no conj), non-unit, N=3', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_lower_trans;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 1;
	Av[ 1 ] = 1;
	Av[ 2 ] = 2;
	Av[ 3 ] = 1;
	Av[ 4 ] = 3;
	Av[ 5 ] = 1;
	Av[ 10 ] = 4;
	Av[ 11 ] = 2;
	Av[ 12 ] = 5;
	Av[ 13 ] = 2;
	Av[ 20 ] = 6;
	Av[ 21 ] = 3;
	x = new Complex128Array( [ 1, 0, 2, 3, 4, 5 ] );
	ztrsv( 'lower', 'transpose', 'non-unit', 3, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: negative stride incx=-1, lower, no transpose, N=2', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_neg_stride;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 2;
	Av[ 1 ] = 0;
	Av[ 2 ] = 1;
	Av[ 3 ] = 1;
	Av[ 10 ] = 3;
	Av[ 11 ] = 0;
	x = new Complex128Array( [ 4, 0, 5, 1 ] );
	ztrsv( 'lower', 'no-transpose', 'non-unit', 2, A, 1, 4, 0, x, -1, 1 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, unit diag, no transpose, N=3', function t() {
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_lower_unit;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 99;
	Av[ 1 ] = 99;
	Av[ 2 ] = 1;
	Av[ 3 ] = 0;
	Av[ 4 ] = 2;
	Av[ 5 ] = 1;
	Av[ 10 ] = 99;
	Av[ 11 ] = 99;
	Av[ 12 ] = 3;
	Av[ 13 ] = 0;
	Av[ 20 ] = 99;
	Av[ 21 ] = 99;
	x = new Complex128Array( [ 1, 0, 3, 1, 10, 5 ] );
	ztrsv( 'lower', 'no-transpose', 'unit', 3, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: upper, conj-trans, Smith else-branch (|imag|>|real| diagonal)', function t() { // eslint-disable-line max-len
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_upper_conjtrans_smith;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 1;
	Av[ 1 ] = 5;
	Av[ 8 ] = 2;
	Av[ 9 ] = 1;
	Av[ 10 ] = 1;
	Av[ 11 ] = 4;
	x = new Complex128Array( [ 3, 2, 1, 1 ] );
	ztrsv( 'upper', 'conjugate-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, conj-trans, Smith else-branch (|imag|>|real| diagonal)', function t() { // eslint-disable-line max-len
	var tc;
	var Av;
	var xv;
	var A;
	var x;

	tc = ztrsv_lower_conjtrans_smith;
	A = new Complex128Array( 4 * 4 );
	Av = reinterpret( A, 0 );
	Av[ 0 ] = 1;
	Av[ 1 ] = 5;
	Av[ 2 ] = 2;
	Av[ 3 ] = 1;
	Av[ 10 ] = 1;
	Av[ 11 ] = 4;
	x = new Complex128Array( [ 3, 2, 1, 1 ] );
	ztrsv( 'lower', 'conjugate-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	xv = reinterpret( x, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
});

// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var A = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'foo', 'no-transpose', 'non-unit', 2, A, 1, 2, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid trans', function t() {
	var A = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'foo', 'non-unit', 2, A, 1, 2, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var A = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'foo', 2, A, 1, 2, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var A = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', -1, A, 1, 2, 0, x, 1, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for strideX=0', function t() {
	var A = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', 2, A, 1, 2, 0, x, 0, 0 );
	}, RangeError );
});

test( 'ndarray: N=0 early return', function t() {
	var out;
	var xv;
	var A;
	var x;

	A = new Complex128Array( 1 );
	x = new Complex128Array( [ 5, 5 ] );
	out = ndarray( 'upper', 'no-transpose', 'non-unit', 0, A, 1, 1, 0, x, 1, 0 );
	assert.strictEqual( out, x );
	xv = reinterpret( x, 0 );
	assert.strictEqual( xv[ 0 ], 5 );
	assert.strictEqual( xv[ 1 ], 5 );
});
