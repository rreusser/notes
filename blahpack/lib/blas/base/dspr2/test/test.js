/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dspr2 = require( './../lib/base.js' );

// FIXTURES //

var upper_basic = require( './fixtures/upper_basic.json' );
var lower_basic = require( './fixtures/lower_basic.json' );
var upper_alpha = require( './fixtures/upper_alpha.json' );
var lower_alpha = require( './fixtures/lower_alpha.json' );
var n_zero = require( './fixtures/n_zero.json' );
var alpha_zero = require( './fixtures/alpha_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var upper_stride = require( './fixtures/upper_stride.json' );
var lower_stride = require( './fixtures/lower_stride.json' );
var upper_4x4 = require( './fixtures/upper_4x4.json' );
var lower_4x4 = require( './fixtures/lower_4x4.json' );
var upper_zeros = require( './fixtures/upper_zeros.json' );

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

test( 'dspr2: upper_basic (uplo=U, N=3, alpha=1, unit strides)', function t() {
	var tc = upper_basic;
	var AP = new Float64Array( [ 1, 2, 5, 3, 6, 9 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var y = new Float64Array( [ 4, 5, 6 ] );

	dspr2( 'upper', 3, 1.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: lower_basic (uplo=L, N=3, alpha=1, unit strides)', function t() {
	var tc = lower_basic;
	var AP = new Float64Array( [ 1, 2, 3, 5, 6, 9 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var y = new Float64Array( [ 4, 5, 6 ] );

	dspr2( 'lower', 3, 1.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: upper_alpha (uplo=U, N=3, alpha=2.5)', function t() {
	var tc = upper_alpha;
	var AP = new Float64Array( [ 1, 2, 5, 3, 6, 9 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var y = new Float64Array( [ 0.5, 1.5, 2.5 ] );

	dspr2( 'upper', 3, 2.5, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: lower_alpha (uplo=L, N=3, alpha=0.5)', function t() {
	var tc = lower_alpha;
	var AP = new Float64Array( [ 1, 2, 3, 5, 6, 9 ] );
	var x = new Float64Array( [ 2, 3, 4 ] );
	var y = new Float64Array( [ 1, -1, 2 ] );

	dspr2( 'lower', 3, 0.5, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: n_zero (quick return)', function t() {
	var tc = n_zero;
	var AP = new Float64Array( [ 99 ] );
	var x = new Float64Array( [ 2 ] );
	var y = new Float64Array( [ 1 ] );

	dspr2( 'upper', 0, 1.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: alpha_zero (quick return)', function t() {
	var tc = alpha_zero;
	var AP = new Float64Array( [ 99 ] );
	var x = new Float64Array( [ 2 ] );
	var y = new Float64Array( [ 1 ] );

	dspr2( 'upper', 3, 0.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: n_one (N=1)', function t() {
	var tc = n_one;
	var AP = new Float64Array( [ 5 ] );
	var x = new Float64Array( [ 3 ] );
	var y = new Float64Array( [ 2 ] );

	dspr2( 'upper', 1, 1.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: upper_stride (uplo=U, N=3, incx=2, incy=2)', function t() {
	var tc = upper_stride;
	var AP = new Float64Array( [ 1, 2, 5, 3, 6, 9 ] );
	var x = new Float64Array( [ 1, 0, 2, 0, 3, 0 ] );
	var y = new Float64Array( [ 4, 0, 5, 0, 6, 0 ] );

	dspr2( 'upper', 3, 1.0, x, 2, 0, y, 2, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: lower_stride (uplo=L, N=3, incx=2, incy=3)', function t() {
	var tc = lower_stride;
	var AP = new Float64Array( [ 1, 2, 3, 5, 6, 9 ] );
	var x = new Float64Array( [ 1, 0, 2, 0, 3, 0 ] );
	var y = new Float64Array( [ 4, 0, 0, 5, 0, 0, 6, 0, 0 ] );

	dspr2( 'lower', 3, 1.0, x, 2, 0, y, 3, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: upper_4x4 (uplo=U, N=4)', function t() {
	var tc = upper_4x4;
	var AP = new Float64Array( [ 1, 2, 5, 3, 6, 8, 4, 7, 9, 10 ] );
	var x = new Float64Array( [ 1, -1, 2, -2 ] );
	var y = new Float64Array( [ 3, 0.5, -1, 1.5 ] );

	dspr2( 'upper', 4, 1.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: lower_4x4 (uplo=L, N=4)', function t() {
	var tc = lower_4x4;
	var AP = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
	var x = new Float64Array( [ 1, -1, 2, -2 ] );
	var y = new Float64Array( [ 3, 0.5, -1, 1.5 ] );

	dspr2( 'lower', 4, 1.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: upper_zeros (skip branch when x[j]=0 and y[j]=0)', function t() {
	var tc = upper_zeros;
	var AP = new Float64Array( [ 1, 2, 5, 3, 6, 9 ] );
	var x = new Float64Array( [ 0, 2, 0 ] );
	var y = new Float64Array( [ 0, 5, 0 ] );

	dspr2( 'upper', 3, 1.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
});

test( 'dspr2: returns AP', function t() {
	var result;
	var AP;
	var x;
	var y;

	AP = new Float64Array( [ 1 ] );
	x = new Float64Array( [ 1 ] );
	y = new Float64Array( [ 1 ] );
	result = dspr2( 'upper', 1, 1.0, x, 1, 0, y, 1, 0, AP, 1, 0 );
	assert.equal( result, AP );
});
