/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlargv = require( './../lib/base.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var all_y_zero = require( './fixtures/all_y_zero.json' );
var all_x_zero = require( './fixtures/all_x_zero.json' );
var stride = require( './fixtures/stride.json' );
var imag_only_x = require( './fixtures/imag_only_x.json' );
var large_values = require( './fixtures/large_values.json' );
var small_values = require( './fixtures/small_values.json' );
var both_zero = require( './fixtures/both_zero.json' );
var f_small_vs_g = require( './fixtures/f_small_vs_g.json' );
var f_large_vs_g = require( './fixtures/f_large_vs_g.json' );
var negative_y = require( './fixtures/negative_y.json' );
var f_zero_g_complex = require( './fixtures/f_zero_g_complex.json' );
var f_small_abs1_gt_one = require( './fixtures/f_small_abs1_gt_one.json' );
var f_small_abs1_le_one = require( './fixtures/f_small_abs1_le_one.json' );
var very_large = require( './fixtures/very_large.json' );
var overflow_common_path = require( './fixtures/overflow_common_path.json' );

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
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Returns an array of values from a Float64Array as a regular array.
*
* @private
* @param {Float64Array} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i += 1 ) {
		out.push( arr[ i ] );
	}
	return out;
}

// TESTS //

test( 'zlargv is a function', function t() {
	assert.equal( typeof zlargv, 'function' );
});

test( 'zlargv: basic (mixed cases in one call)', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = basic;
	x = new Complex128Array( [ 3.0, 1.0, 0.0, 0.0, 1.0, 2.0, 0.0, 1.0 ] );
	y = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, 3.0, 1.0, 2.0, -1.0 ] );
	c = new Float64Array( 4 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 4, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: n=0 (no-op)', function t() {
	var xv;
	var yv;
	var x;
	var y;
	var c;

	x = new Complex128Array( [ 99.0, 99.0 ] );
	y = new Complex128Array( [ 99.0, 99.0 ] );
	c = new Float64Array( [ 99.0 ] );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 0, x, 1, 0, y, 1, 0, c, 1, 0 );
	assert.equal( xv[ 0 ], 99.0 );
	assert.equal( xv[ 1 ], 99.0 );
	assert.equal( yv[ 0 ], 99.0 );
	assert.equal( yv[ 1 ], 99.0 );
	assert.equal( c[ 0 ], 99.0 );
});

test( 'zlargv: all y=0 (cosines=1, sines=0)', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = all_y_zero;
	x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 0.0 ] );
	y = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );
	c = new Float64Array( 3 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 3, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: all x=0', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = all_x_zero;
	x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );
	y = new Complex128Array( [ 5.0, 0.0, 3.0, 4.0, 0.0, 7.0 ] );
	c = new Float64Array( 3 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 3, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: non-unit strides', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = stride;
	x = new Complex128Array( [ 3.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0 ] ); // eslint-disable-line max-len
	y = new Complex128Array( [ 4.0, 0.0, 0.0, 0.0, 7.0, 0.0, 0.0, 0.0, 3.0, 1.0 ] ); // eslint-disable-line max-len
	c = new Float64Array( 6 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 3, x, 2, 0, y, 2, 0, c, 2, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: x with only imaginary parts', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = imag_only_x;
	x = new Complex128Array( [ 0.0, 3.0, 0.0, 5.0 ] );
	y = new Complex128Array( [ 4.0, 0.0, 12.0, 0.0 ] );
	c = new Float64Array( 2 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 2, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: large values (near overflow rescaling)', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = large_values;
	x = new Complex128Array( [ 1e150, 1e150, 1e-150, 1e-150 ] );
	y = new Complex128Array( [ 1e150, 0.0, 1e-150, 0.0 ] );
	c = new Float64Array( 2 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 2, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: small values (near underflow rescaling)', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = small_values;
	x = new Complex128Array( [ 1e-300, 2e-300 ] );
	y = new Complex128Array( [ 3e-300, 1e-300 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: both zero', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = both_zero;
	x = new Complex128Array( [ 0.0, 0.0 ] );
	y = new Complex128Array( [ 0.0, 0.0 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: |f| small relative to |g|', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = f_small_vs_g;
	x = new Complex128Array( [ 1e-200, 1e-200 ] );
	y = new Complex128Array( [ 1.0, 0.0 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: |f| >> |g| (normal path)', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = f_large_vs_g;
	x = new Complex128Array( [ 10.0, 5.0 ] );
	y = new Complex128Array( [ 1.0, 0.0 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: negative y', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = negative_y;
	x = new Complex128Array( [ 2.0, 3.0 ] );
	y = new Complex128Array( [ -4.0, 2.0 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: f=0, g complex', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = f_zero_g_complex;
	x = new Complex128Array( [ 0.0, 0.0 ] );
	y = new Complex128Array( [ 3.0, 4.0 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: f small, ABS1(f) > 1', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = f_small_abs1_gt_one;
	x = new Complex128Array( [ 2.0, 0.0 ] );
	y = new Complex128Array( [ 1e200, 0.0 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: f small, ABS1(f) <= 1', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = f_small_abs1_le_one;
	x = new Complex128Array( [ 0.5, 0.3 ] );
	y = new Complex128Array( [ 1e200, 0.0 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: very large values (overflow scaling)', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = very_large;
	x = new Complex128Array( [ 1e200, 1e200 ] );
	y = new Complex128Array( [ 1e200, 1e200 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-12, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: overflow scaling, common path (count > 0)', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = overflow_common_path;
	x = new Complex128Array( [ 1e250, 0.0 ] );
	y = new Complex128Array( [ 1e200, 0.0 ] );
	c = new Float64Array( 1 );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 0, y, 1, 0, c, 1, 0 );
	assertArrayClose( toArray( xv ), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( yv ), tc.y, 1e-14, 'y' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
});

test( 'zlargv: offset support', function t() {
	var tc;
	var xv;
	var yv;
	var x;
	var y;
	var c;

	tc = basic;
	x = new Complex128Array( [ 999.0, 999.0, 3.0, 1.0 ] );
	y = new Complex128Array( [ 999.0, 999.0, 0.0, 0.0 ] );
	c = new Float64Array( [ 999.0, 0.0 ] );
	xv = reinterpret( x, 0 );
	yv = reinterpret( y, 0 );
	zlargv( 1, x, 1, 1, y, 1, 1, c, 1, 1 );
	assert.equal( xv[ 0 ], 999.0 );
	assert.equal( xv[ 1 ], 999.0 );
	assert.equal( yv[ 0 ], 999.0 );
	assert.equal( yv[ 1 ], 999.0 );
	assert.equal( c[ 0 ], 999.0 );
	assertClose( c[ 1 ], tc.c[ 0 ], 1e-14, 'c[1]' );
	assertClose( xv[ 2 ], tc.x[ 0 ], 1e-14, 'x re' );
	assertClose( xv[ 3 ], tc.x[ 1 ], 1e-14, 'x im' );
});

test( 'zlargv: mathematical property (rotation zeros out y)', function t() {
	var xOrigR;
	var xOrigI;
	var yOrigR;
	var yOrigI;
	var resR;
	var resI;
	var yv;
	var sr;
	var si;
	var ci;
	var x;
	var y;
	var c;
	var i;

	x = new Complex128Array( [ 3.0, 1.0, 0.0, 0.0, 1.0, 2.0, 2.0, 3.0 ] );
	y = new Complex128Array( [ 4.0, 0.0, 5.0, 0.0, 3.0, 1.0, -4.0, 2.0 ] );
	c = new Float64Array( 4 );
	yv = reinterpret( y, 0 );
	xOrigR = [ 3.0, 0.0, 1.0, 2.0 ];
	xOrigI = [ 1.0, 0.0, 2.0, 3.0 ];
	yOrigR = [ 4.0, 5.0, 3.0, -4.0 ];
	yOrigI = [ 0.0, 0.0, 1.0, 2.0 ];
	zlargv( 4, x, 1, 0, y, 1, 0, c, 1, 0 );
	for ( i = 0; i < 4; i += 1 ) {
		// The rotation is:
		// ( c      s  ) ( x_orig ) = ( r )
		// (-conj(s) c ) ( y_orig ) = ( 0 )
		// Check: -conj(s)*x_orig + c*y_orig should be ~0
		sr = yv[ i * 2 ];
		si = yv[ ( i * 2 ) + 1 ];
		ci = c[ i ];

		// -conj(s)*x_orig + c*y_orig
		resR = ( -sr * xOrigR[ i ] ) - ( si * xOrigI[ i ] ) + ( ci * yOrigR[ i ] ); // eslint-disable-line max-len
		resI = ( si * xOrigR[ i ] ) - ( sr * xOrigI[ i ] ) + ( ci * yOrigI[ i ] ); // eslint-disable-line max-len

		assert.ok( Math.abs( resR ) < 1e-12, 'rotation zeros out y real part [' + i + ']: residual=' + resR ); // eslint-disable-line max-len
		assert.ok( Math.abs( resI ) < 1e-12, 'rotation zeros out y imag part [' + i + ']: residual=' + resI ); // eslint-disable-line max-len
	}
});
