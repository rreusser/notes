

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zspmv = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_basic = require( './fixtures/upper_basic.json' );
var lower_basic = require( './fixtures/lower_basic.json' );
var complex_alpha_beta = require( './fixtures/complex_alpha_beta.json' );
var alpha_zero = require( './fixtures/alpha_zero.json' );
var n_zero = require( './fixtures/n_zero.json' );
var alpha_zero_beta_zero = require( './fixtures/alpha_zero_beta_zero.json' );
var stride_2 = require( './fixtures/stride_2.json' );
var scalar = require( './fixtures/scalar.json' );
var lower_nonzero_beta = require( './fixtures/lower_nonzero_beta.json' );
var negative_incx = require( './fixtures/negative_incx.json' );
var beta_one = require( './fixtures/beta_one.json' );
var lower_stride2_complex_beta = require( './fixtures/lower_stride2_complex_beta.json' );

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

// 3x3 complex symmetric matrix (packed upper):
// A = [2+i    1+i   3-2i]
//     [1+i    4-i   2+i ]
//     [3-2i   2+i   5+3i]
function upperAP() {
	return new Complex128Array( [ 2, 1, 1, 1, 4, -1, 3, -2, 2, 1, 5, 3 ] );
}

// Same matrix packed lower:
function lowerAP() {
	return new Complex128Array( [ 2, 1, 1, 1, 3, -2, 4, -1, 2, 1, 5, 3 ] );
}

function stdX() {
	return new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
}

// TESTS //

test( 'zspmv is a function', function t() {
	assert.equal( typeof zspmv, 'function' );
});

test( 'zspmv: upper_basic', function t() {
	var tc = upper_basic;
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( 3 );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: lower_basic', function t() {
	var tc = lower_basic;
	var ap = lowerAP();
	var x = stdX();
	var y = new Complex128Array( 3 );
	var result = zspmv( 'lower', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: complex_alpha_beta', function t() {
	var tc = complex_alpha_beta;
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 1, 2, -1, 0.5, 0.5 ] );
	var result = zspmv( 'upper', 3, new Complex128( 2, 1 ), ap, 1, 0, x, 1, 0, new Complex128( 0.5, -0.5 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: alpha_zero', function t() {
	var tc = alpha_zero;
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var result = zspmv( 'upper', 3, new Complex128( 0, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 2, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: n_zero', function t() {
	var tc = n_zero;
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 99, 0 ] );
	var result = zspmv( 'upper', 0, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: alpha_zero_beta_zero', function t() {
	var tc = alpha_zero_beta_zero;
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 99, 88, 77, 66, 55, 44 ] );
	var result = zspmv( 'upper', 3, new Complex128( 0, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: stride_2', function t() {
	var tc = stride_2;
	var ap = upperAP();
	var x = new Complex128Array( [ 1, 0.5, 0, 0, 2, -1, 0, 0, 3, 1, 0, 0 ] );
	var y = new Complex128Array( 6 );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 2, 0, new Complex128( 0, 0 ), y, 2, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: scalar', function t() {
	var tc = scalar;
	var ap = new Complex128Array( [ 3, 2 ] );
	var x = new Complex128Array( [ 5, 2 ] );
	var y = new Complex128Array( 1 );
	var result = zspmv( 'upper', 1, new Complex128( 2, 1 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: lower_nonzero_beta', function t() {
	var tc = lower_nonzero_beta;
	var ap = lowerAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 1, 2, -1, 0.5, 0.5 ] );
	var result = zspmv( 'lower', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0.5, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: negative_incx', function t() {
	var tc = negative_incx;
	// Fortran: x(1)=3+i, x(2)=2-i, x(3)=1+0.5i, incx=-1
	// With negative stride, start from end: elements are reversed
	var ap = upperAP();
	var x = new Complex128Array( [ 3, 1, 2, -1, 1, 0.5 ] );
	var y = new Complex128Array( 3 );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, -1, 2, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: beta_one', function t() {
	var tc = beta_one;
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 2, 3, -1, 2, 1 ] );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 1, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: lower_stride2_complex_beta', function t() {
	var tc = lower_stride2_complex_beta;
	var ap = lowerAP();
	var x = new Complex128Array( [ 1, 0.5, 0, 0, 2, -1, 0, 0, 3, 1, 0, 0 ] );
	var y = new Complex128Array( [ 1, 1, 0, 0, 2, -1, 0, 0, 0.5, 0.5, 0, 0 ] );
	var result = zspmv( 'lower', 3, new Complex128( 2, 1 ), ap, 1, 0, x, 2, 0, new Complex128( 0.5, -0.5 ), y, 2, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: alpha_zero_beta_one quick return', function t() {
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var result = zspmv( 'upper', 3, new Complex128( 0, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 1, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	// y should be unchanged
	assertArrayClose( Array.from( view ), [ 1, 2, 3, 4, 5, 6 ], 1e-14, 'y' );
});

test( 'zspmv: returns y', function t() {
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( 3 );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	assert.equal( result, y );
});
