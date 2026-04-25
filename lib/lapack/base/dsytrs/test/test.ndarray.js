/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsytrf = require( '../../dsytrf/lib/base.js' );
var dsytrs = require( './../lib/ndarray.js' );

// FIXTURES //

var _4x4_lower_1rhs = require( './fixtures/4x4_lower_1rhs.json' );
var _4x4_upper_1rhs = require( './fixtures/4x4_upper_1rhs.json' );
var _4x4_indef_lower_1rhs = require( './fixtures/4x4_indef_lower_1rhs.json' );
var _4x4_indef_upper_1rhs = require( './fixtures/4x4_indef_upper_1rhs.json' );
var _3x3_lower_2rhs = require( './fixtures/3x3_lower_2rhs.json' );
var n_one_lower = require( './fixtures/n_one_lower.json' );
var _5x5_lower_solve = require( './fixtures/5x5_lower_solve.json' );
var _5x5_upper_solve = require( './fixtures/5x5_upper_solve.json' );

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

// TESTS //

test( 'dsytrs: 4x4_lower_1rhs', function t() {
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = _4x4_lower_1rhs;
	A = new Float64Array([
		4,
		2,
		1,
		0,
		0,
		5,
		2,
		1,
		0,
		0,
		6,
		3,
		0,
		0,
		0,
		8
	]);
	b = new Float64Array([ 7, 10, 12, 12 ]);
	ipiv = new Int32Array( 4 );
	dsytrf( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	info = dsytrs( 'lower', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 4x4_upper_1rhs', function t() {
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = _4x4_upper_1rhs;
	A = new Float64Array( 16 );
	A[ 0 ] = 4;
	A[ 4 ] = 2;
	A[ 5 ] = 5;
	A[ 8 ] = 1;
	A[ 9 ] = 2;
	A[ 10 ] = 6;
	A[ 12 ] = 0;
	A[ 13 ] = 1;
	A[ 14 ] = 3;
	A[ 15 ] = 8;
	b = new Float64Array([ 7, 10, 12, 12 ]);
	ipiv = new Int32Array( 4 );
	dsytrf( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	info = dsytrs( 'upper', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 4x4_indef_lower_1rhs', function t() {
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = _4x4_indef_lower_1rhs;
	A = new Float64Array([
		0,
		1,
		2,
		3,
		0,
		0,
		4,
		5,
		0,
		0,
		0,
		6,
		0,
		0,
		0,
		0
	]);
	b = new Float64Array([ 6, 10, 12, 14 ]);
	ipiv = new Int32Array( 4 );
	dsytrf( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	info = dsytrs( 'lower', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 4x4_indef_upper_1rhs', function t() {
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = _4x4_indef_upper_1rhs;
	A = new Float64Array( 16 );
	A[ 0 ] = 0;
	A[ 4 ] = 1;
	A[ 5 ] = 0;
	A[ 8 ] = 2;
	A[ 9 ] = 4;
	A[ 10 ] = 0;
	A[ 12 ] = 3;
	A[ 13 ] = 5;
	A[ 14 ] = 6;
	A[ 15 ] = 0;
	b = new Float64Array([ 6, 10, 12, 14 ]);
	ipiv = new Int32Array( 4 );
	dsytrf( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	info = dsytrs( 'upper', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 3x3_lower_2rhs', function t() {
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = _3x3_lower_2rhs;
	A = new Float64Array([
		4,
		2,
		1,
		0,
		5,
		2,
		0,
		0,
		6
	]);
	b = new Float64Array([ 7, 9, 9, 14, 18, 18 ]);
	ipiv = new Int32Array( 3 );
	dsytrf( 'lower', 3, A, 1, 3, 0, ipiv, 1, 0 );
	info = dsytrs( 'lower', 3, 2, A, 1, 3, 0, ipiv, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: n_zero', function t() {
	var ipiv;
	var info;
	var A;
	var b;

	A = new Float64Array( 1 );
	b = new Float64Array( 1 );
	ipiv = new Int32Array( 1 );
	info = dsytrs( 'lower', 0, 1, A, 1, 1, 0, ipiv, 1, 0, b, 1, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytrs: nrhs_zero', function t() {
	var ipiv;
	var info;
	var A;
	var b;

	A = new Float64Array( 9 );
	b = new Float64Array( 3 );
	ipiv = new Int32Array( 3 );
	info = dsytrs( 'lower', 3, 0, A, 1, 3, 0, ipiv, 1, 0, b, 1, 3, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytrs: n_one_lower', function t() {
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = n_one_lower;
	A = new Float64Array([ 4.0 ]);
	b = new Float64Array([ 8.0 ]);
	ipiv = new Int32Array([ 0 ]);
	info = dsytrs( 'lower', 1, 1, A, 1, 1, 0, ipiv, 1, 0, b, 1, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-14, 'b' );
});

test( 'dsytrs: 5x5_lower_solve', function t() {
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = _5x5_lower_solve;
	A = new Float64Array([
		1,
		-2,
		0,
		3,
		1,
		0,
		0,
		4,
		-1,
		2,
		0,
		0,
		-3,
		2,
		0,
		0,
		0,
		0,
		1,
		-2,
		0,
		0,
		0,
		0,
		4
	]);
	b = new Float64Array([ 14, 16, 7, 1, 17 ]);
	ipiv = new Int32Array( 5 );
	dsytrf( 'lower', 5, A, 1, 5, 0, ipiv, 1, 0 );
	info = dsytrs( 'lower', 5, 1, A, 1, 5, 0, ipiv, 1, 0, b, 1, 5, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 5x5_upper_solve', function t() {
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = _5x5_upper_solve;
	A = new Float64Array( 25 );
	A[ 0 ] = 1;
	A[ 5 ] = -2;
	A[ 6 ] = 0;
	A[ 10 ] = 0;
	A[ 11 ] = 4;
	A[ 12 ] = -3;
	A[ 15 ] = 3;
	A[ 16 ] = -1;
	A[ 17 ] = 2;
	A[ 18 ] = 1;
	A[ 20 ] = 1;
	A[ 21 ] = 2;
	A[ 22 ] = 0;
	A[ 23 ] = -2;
	A[ 24 ] = 4;
	b = new Float64Array([ 14, 16, 7, 1, 17 ]);
	ipiv = new Int32Array( 5 );
	dsytrf( 'upper', 5, A, 1, 5, 0, ipiv, 1, 0 );
	info = dsytrs( 'upper', 5, 1, A, 1, 5, 0, ipiv, 1, 0, b, 1, 5, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});
