/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgetrf2 = require( './../lib/base.js' );

// FIXTURES //

var _3x3 = require( './fixtures/3x3.json' );
var _4x3 = require( './fixtures/4x3.json' );
var _3x4 = require( './fixtures/3x4.json' );
var singular = require( './fixtures/singular.json' );
var _1x1 = require( './fixtures/1x1.json' );
var col_vector = require( './fixtures/col_vector.json' );
var row_vector = require( './fixtures/row_vector.json' );

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
* Converts 1-based Fortran IPIV values to 0-based JS IPIV values.
*/
function ipivTo0Based( ipiv1 ) {
	var out = new Int32Array( ipiv1.length );
	var i;
	for ( i = 0; i < ipiv1.length; i++ ) {
		out[ i ] = ipiv1[ i ] - 1;
	}
	return out;
}

// TESTS //

test( 'dgetrf2: 3x3 non-singular matrix', function t() {
	var expectedIPIV;
	var ipiv;
	var info;
	var tc;
	var a;

	tc = _3x3;
	a = new Float64Array( [ 2, 4, 8, 1, 3, 7, 1, 3, 9 ] );
	ipiv = new Int32Array( 3 );
	info = dgetrf2( 3, 3, a, 1, 3, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	expectedIPIV = ipivTo0Based( tc.ipiv );
	assert.deepEqual( ipiv, expectedIPIV, 'ipiv' );
});

test( 'dgetrf2: 4x3 tall matrix', function t() {
	var ipiv;
	var info;
	var tc;
	var a;

	tc = _4x3;
	a = new Float64Array( [ 2, 0, 1, 0, 1, 3, 0, 1, 0, 1, 4, 2 ] );
	ipiv = new Int32Array( 3 );
	info = dgetrf2( 4, 3, a, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: 3x4 wide matrix', function t() {
	var ipiv;
	var info;
	var tc;
	var a;

	tc = _3x4;
	a = new Float64Array( [ 1, 4, 7, 2, 5, 8, 3, 6, 9, 10, 11, 12 ] );
	ipiv = new Int32Array( 3 );
	info = dgetrf2( 3, 4, a, 1, 3, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: singular matrix (info > 0)', function t() {
	var ipiv;
	var info;
	var tc;
	var a;

	tc = singular;
	a = new Float64Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1 ] );
	ipiv = new Int32Array( 3 );
	info = dgetrf2( 3, 3, a, 1, 3, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info should be 2 (singular at column 2)' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: N=0 quick return', function t() {
	var ipiv;
	var info;
	var a;

	a = new Float64Array( [ 99 ] );
	ipiv = new Int32Array( 1 );
	info = dgetrf2( 3, 0, a, 1, 3, 0, ipiv, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dgetrf2: M=0 quick return', function t() {
	var ipiv;
	var info;
	var a;

	a = new Float64Array( 1 );
	ipiv = new Int32Array( 1 );
	info = dgetrf2( 0, 3, a, 1, 1, 0, ipiv, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dgetrf2: 1x1 non-singular', function t() {
	var ipiv;
	var info;
	var tc;
	var a;

	tc = _1x1;
	a = new Float64Array( [ 5 ] );
	ipiv = new Int32Array( 1 );
	info = dgetrf2( 1, 1, a, 1, 1, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: 1x1 singular (zero)', function t() {
	var ipiv;
	var info;
	var a;

	a = new Float64Array( [ 0 ] );
	ipiv = new Int32Array( 1 );
	info = dgetrf2( 1, 1, a, 1, 1, 0, ipiv, 1, 0 );
	assert.equal( a[ 0 ], 0.0, 'a[0] should be 0' );
	assert.equal( info, 1, 'info should be 1' );
	assert.equal( ipiv[ 0 ], 0, 'ipiv[0] should be 0 (0-based)' );
});

test( 'dgetrf2: Nx1 column vector', function t() {
	var ipiv;
	var info;
	var tc;
	var a;

	tc = col_vector;
	a = new Float64Array( [ 1, 5, 3 ] );
	ipiv = new Int32Array( 1 );
	info = dgetrf2( 3, 1, a, 1, 3, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: 1xN row vector', function t() {
	var ipiv;
	var info;
	var tc;
	var a;

	tc = row_vector;
	a = new Float64Array( [ 2, 3, 7 ] );
	ipiv = new Int32Array( 1 );
	info = dgetrf2( 1, 3, a, 1, 1, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: Nx1 column with subnormal pivot (sfmin path)', function t() {
	var ipiv2;
	var info2;
	var ipiv;
	var info;
	var a2;
	var a;

	a = new Float64Array( [ 5e-324, 1.0, 2.0 ] );
	ipiv = new Int32Array( 1 );
	info = dgetrf2( 3, 1, a, 1, 3, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'info should be 0' );
	a2 = new Float64Array( [ 1e-310, 5e-311, 2e-311 ] );
	ipiv2 = new Int32Array( 1 );
	info2 = dgetrf2( 3, 1, a2, 1, 3, 0, ipiv2, 1, 0 );
	assert.equal( info2, 0, 'info should be 0' );
	assert.equal( ipiv2[ 0 ], 0, 'pivot should be row 0 (largest element)' );
	assertClose( a2[ 1 ], 5e-311 / 1e-310, 1e-10, 'a2[1]' );
	assertClose( a2[ 2 ], 2e-311 / 1e-310, 1e-10, 'a2[2]' );
});
