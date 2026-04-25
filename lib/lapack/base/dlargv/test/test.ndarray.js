/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlargv = require( './../lib/ndarray.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var n_one_f_gt_g = require( './fixtures/n_one_f_gt_g.json' );
var n_one_f_lt_g = require( './fixtures/n_one_f_lt_g.json' );
var all_g_zero = require( './fixtures/all_g_zero.json' );
var all_f_zero = require( './fixtures/all_f_zero.json' );
var stride = require( './fixtures/stride.json' );
var negative = require( './fixtures/negative.json' );
var equal_mag = require( './fixtures/equal_mag.json' );
var large_values = require( './fixtures/large_values.json' );

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

test( 'dlargv is a function', function t() {
	assert.equal( typeof dlargv, 'function' );
});

test( 'dlargv: basic (mixed cases in one call)', function t() {
	var tc = basic;
	var x = new Float64Array( [ 3.0, 0.0, 5.0, 1.0 ] );
	var y = new Float64Array( [ 0.0, 4.0, 3.0, 2.0 ] );
	var c = new Float64Array( 4 );
	dlargv( 4, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
});

test( 'dlargv: n=0 (no-op)', function t() {
	var x = new Float64Array( [ 99.0 ] );
	var y = new Float64Array( [ 99.0 ] );
	var c = new Float64Array( [ 99.0 ] );
	dlargv( 0, x, 1, 0, y, 1, 0, c, 1, 0 );
	assert.equal( x[ 0 ], 99.0 );
	assert.equal( y[ 0 ], 99.0 );
	assert.equal( c[ 0 ], 99.0 );
});

test( 'dlargv: n=1, |f| > |g|', function t() {
	var tc = n_one_f_gt_g;
	var x = new Float64Array( [ 4.0 ] );
	var y = new Float64Array( [ 3.0 ] );
	var c = new Float64Array( 1 );
	dlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
});

test( 'dlargv: n=1, |f| < |g|', function t() {
	var tc = n_one_f_lt_g;
	var x = new Float64Array( [ 3.0 ] );
	var y = new Float64Array( [ 4.0 ] );
	var c = new Float64Array( 1 );
	dlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
});

test( 'dlargv: all g=0', function t() {
	var tc = all_g_zero;
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var y = new Float64Array( [ 0.0, 0.0, 0.0 ] );
	var c = new Float64Array( 3 );
	dlargv( 3, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
});

test( 'dlargv: all f=0', function t() {
	var tc = all_f_zero;
	var x = new Float64Array( [ 0.0, 0.0, 0.0 ] );
	var y = new Float64Array( [ 5.0, 6.0, 7.0 ] );
	var c = new Float64Array( 3 );
	dlargv( 3, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
});

test( 'dlargv: non-unit strides', function t() {
	var tc = stride;
	var x = new Float64Array( [ 3.0, 0.0, 0.0, 0.0, 5.0, 0.0 ] );
	var y = new Float64Array( [ 4.0, 0.0, 0.0, 7.0, 0.0, 0.0, 12.0, 0.0, 0.0 ] );
	var c = new Float64Array( 6 );
	dlargv( 3, x, 2, 0, y, 3, 0, c, 2, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
});

test( 'dlargv: negative values', function t() {
	var tc = negative;
	var x = new Float64Array( [ -3.0, -5.0, 4.0 ] );
	var y = new Float64Array( [ 4.0, -12.0, -3.0 ] );
	var c = new Float64Array( 3 );
	dlargv( 3, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
});

test( 'dlargv: equal magnitudes', function t() {
	var tc = equal_mag;
	var x = new Float64Array( [ 1.0, -1.0, 3.0 ] );
	var y = new Float64Array( [ 1.0, 1.0, -3.0 ] );
	var c = new Float64Array( 3 );
	dlargv( 3, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
});

test( 'dlargv: large values', function t() {
	var tc = large_values;
	var x = new Float64Array( [ 1.0e200, 1.0e-200 ] );
	var y = new Float64Array( [ 1.0e200, 1.0e-200 ] );
	var c = new Float64Array( 2 );
	dlargv( 2, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
});

test( 'dlargv: offset support', function t() {
	// Test with non-zero offsets
	var x = new Float64Array( [ 999.0, 4.0, 0.0 ] );
	var y = new Float64Array( [ 999.0, 3.0, 5.0 ] );
	var c = new Float64Array( [ 999.0, 0.0, 0.0 ] );
	dlargv( 2, x, 1, 1, y, 1, 1, c, 1, 1 );

	// First pair: f=4, g=3, |f|>|g| => t=3/4=0.75, tt=sqrt(1+0.5625)=1.25

	// c = 1/1.25 = 0.8, y = 0.75*0.8 = 0.6, x = 4*1.25 = 5
	assertClose( x[ 1 ], 5.0, 1e-14, 'x[1]' );
	assertClose( y[ 1 ], 0.6, 1e-14, 'y[1]' );
	assertClose( c[ 1 ], 0.8, 1e-14, 'c[1]' );

	// Second pair: f=0, g=5 => c=0, y=1, x=5
	assertClose( x[ 2 ], 5.0, 1e-14, 'x[2]' );
	assertClose( y[ 2 ], 1.0, 1e-14, 'y[2]' );
	assertClose( c[ 2 ], 0.0, 1e-14, 'c[2]' );

	// Verify offset element was untouched
	assert.equal( x[ 0 ], 999.0 );
	assert.equal( y[ 0 ], 999.0 );
	assert.equal( c[ 0 ], 999.0 );
});

test( 'dlargv: mathematical property (rotation zeros out y)', function t() {
	var residual;
	var xOrig;
	var yOrig;
	var ci;
	var si;
	var x;
	var y;
	var c;
	var i;

	x = new Float64Array( [ 3.0, 7.0, -2.0, 0.0, 0.0 ] );
	y = new Float64Array( [ 4.0, -1.0, 5.0, 8.0, 0.0 ] );
	c = new Float64Array( 5 );
	xOrig = new Float64Array( x );
	yOrig = new Float64Array( y );
	dlargv( 5, x, 1, 0, y, 1, 0, c, 1, 0 );
	for ( i = 0; i < 5; i += 1 ) {
		ci = c[ i ];
		si = y[ i ];

		// The rotation applied to (xOrig, yOrig) should give (x[i], 0):

		// c*xOrig + s*yOrig = x[i]  (the "a" value)

		// -s*xOrig + c*yOrig = 0    (should be zero)
		residual = Math.abs( ( -si * xOrig[ i ] ) + ( ci * yOrig[ i ] ) );
		assert.ok( residual < 1e-12, 'rotation zeros out y[' + i + ']: residual=' + residual ); // eslint-disable-line max-len
	}
});
