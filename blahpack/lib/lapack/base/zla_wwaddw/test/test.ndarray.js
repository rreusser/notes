

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zla_wwaddw = require( './../lib/base.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var negative = require( './fixtures/negative.json' );
var large_values = require( './fixtures/large_values.json' );
var zeros = require( './fixtures/zeros.json' );
var purely_imaginary = require( './fixtures/purely_imaginary.json' );

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

test( 'zla_wwaddw is a function', function t() {
	assert.equal( typeof zla_wwaddw, 'function' );
});

test( 'zla_wwaddw: basic (n=5)', function t() {
	var tc = basic;
	var x = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
	var y = new Complex128Array( [ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0 ] );
	var w = new Complex128Array( [ 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ] );
	zla_wwaddw( 5, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zla_wwaddw: n_zero (arrays unchanged)', function t() {
	var tc = n_zero;
	var x = new Complex128Array( [ 1, 2, 3, 4 ] );
	var y = new Complex128Array( [ 0.1, 0.2, 0.3, 0.4 ] );
	var w = new Complex128Array( [ 10, 20, 30, 40 ] );
	zla_wwaddw( 0, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zla_wwaddw: n_one', function t() {
	var tc = n_one;
	var x = new Complex128Array( [ 1, 2 ] );
	var y = new Complex128Array( [ 0.1, 0.2 ] );
	var w = new Complex128Array( [ 10, 20 ] );
	zla_wwaddw( 1, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zla_wwaddw: negative values', function t() {
	var tc = negative;
	var x = new Complex128Array( [ -1, -2, -3, -4, -5, -6, -7, -8 ] );
	var y = new Complex128Array( [ 0.01, -0.02, 0.03, -0.04, 0.05, -0.06, 0.07, -0.08 ] );
	var w = new Complex128Array( [ 0.5, -0.5, -0.5, 0.5, 1.5, -1.5, -1.5, 1.5 ] );
	zla_wwaddw( 4, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zla_wwaddw: large_values (precision)', function t() {
	var tc = large_values;
	var x = new Complex128Array( [ 1e15, 2e15, 3e15, 4e15, 5e15, 6e15 ] );
	var y = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var w = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	zla_wwaddw( 3, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zla_wwaddw: zeros', function t() {
	var tc = zeros;
	var x = new Complex128Array( [ 0, 0, 0, 0, 0, 0 ] );
	var y = new Complex128Array( [ 0, 0, 0, 0, 0, 0 ] );
	var w = new Complex128Array( [ 0, 0, 0, 0, 0, 0 ] );
	zla_wwaddw( 3, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zla_wwaddw: purely_imaginary', function t() {
	var tc = purely_imaginary;
	var x = new Complex128Array( [ 0, 1, 0, 3, 0, 5 ] );
	var y = new Complex128Array( [ 0, 0.1, 0, 0.3, 0, 0.5 ] );
	var w = new Complex128Array( [ 0, 10, 0, 30, 0, 50 ] );
	zla_wwaddw( 3, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zla_wwaddw: non-unit strides', function t() {
	// stride=2, offset=1: access elements at complex indices 1, 3, 5
	var x = new Complex128Array( [ 999, 999, 1, 2, 999, 999, 3, 4, 999, 999, 5, 6 ] );
	var y = new Complex128Array( [ 999, 999, 0.1, 0.2, 999, 999, 0.3, 0.4, 999, 999, 0.5, 0.6 ] );
	var w = new Complex128Array( [ 999, 999, 10, 20, 999, 999, 30, 40, 999, 999, 50, 60 ] );

	var tcBasic = basic;
	zla_wwaddw( 3, x, 2, 1, y, 2, 1, w, 2, 1 );

	var xv = reinterpret( x, 0 );
	var yv = reinterpret( y, 0 );

	// Elements at complex index 1 (Float64 index 2,3) should match basic[0]
	assertClose( xv[ 2 ], tcBasic.x[ 0 ], 1e-14, 'x[1].re' );
	assertClose( xv[ 3 ], tcBasic.x[ 1 ], 1e-14, 'x[1].im' );
	assertClose( xv[ 6 ], tcBasic.x[ 2 ], 1e-14, 'x[3].re' );
	assertClose( xv[ 7 ], tcBasic.x[ 3 ], 1e-14, 'x[3].im' );
	assertClose( xv[ 10 ], tcBasic.x[ 4 ], 1e-14, 'x[5].re' );
	assertClose( xv[ 11 ], tcBasic.x[ 5 ], 1e-14, 'x[5].im' );

	assertClose( yv[ 2 ], tcBasic.y[ 0 ], 1e-14, 'y[1].re' );
	assertClose( yv[ 3 ], tcBasic.y[ 1 ], 1e-14, 'y[1].im' );
	assertClose( yv[ 6 ], tcBasic.y[ 2 ], 1e-14, 'y[3].re' );
	assertClose( yv[ 7 ], tcBasic.y[ 3 ], 1e-14, 'y[3].im' );
	assertClose( yv[ 10 ], tcBasic.y[ 4 ], 1e-14, 'y[5].re' );
	assertClose( yv[ 11 ], tcBasic.y[ 5 ], 1e-14, 'y[5].im' );

	// Untouched elements should remain as padding
	assert.equal( xv[ 0 ], 999.0, 'x pad 0' );
	assert.equal( xv[ 1 ], 999.0, 'x pad 1' );
	assert.equal( xv[ 4 ], 999.0, 'x pad 4' );
	assert.equal( xv[ 5 ], 999.0, 'x pad 5' );
});

test( 'zla_wwaddw: negative strides', function t() {
	// stride=-1, offset=2: starts at complex element 2 and goes backward
	var x = new Complex128Array( [ 5, 6, 3, 4, 1, 2 ] );
	var y = new Complex128Array( [ 0.5, 0.6, 0.3, 0.4, 0.1, 0.2 ] );
	var w = new Complex128Array( [ 50, 60, 30, 40, 10, 20 ] );

	var tcBasic = basic;
	zla_wwaddw( 3, x, -1, 2, y, -1, 2, w, -1, 2 );

	var xv = reinterpret( x, 0 );
	var yv = reinterpret( y, 0 );

	// Element at complex index 2 is processed first (x=(1,2), w=(10,20)) => same as basic[0]
	assertClose( xv[ 4 ], tcBasic.x[ 0 ], 1e-14, 'x[2].re' );
	assertClose( xv[ 5 ], tcBasic.x[ 1 ], 1e-14, 'x[2].im' );
	assertClose( xv[ 2 ], tcBasic.x[ 2 ], 1e-14, 'x[1].re' );
	assertClose( xv[ 3 ], tcBasic.x[ 3 ], 1e-14, 'x[1].im' );
	assertClose( xv[ 0 ], tcBasic.x[ 4 ], 1e-14, 'x[0].re' );
	assertClose( xv[ 1 ], tcBasic.x[ 5 ], 1e-14, 'x[0].im' );

	assertClose( yv[ 4 ], tcBasic.y[ 0 ], 1e-14, 'y[2].re' );
	assertClose( yv[ 5 ], tcBasic.y[ 1 ], 1e-14, 'y[2].im' );
	assertClose( yv[ 2 ], tcBasic.y[ 2 ], 1e-14, 'y[1].re' );
	assertClose( yv[ 3 ], tcBasic.y[ 3 ], 1e-14, 'y[1].im' );
	assertClose( yv[ 0 ], tcBasic.y[ 4 ], 1e-14, 'y[0].re' );
	assertClose( yv[ 1 ], tcBasic.y[ 5 ], 1e-14, 'y[0].im' );
});
