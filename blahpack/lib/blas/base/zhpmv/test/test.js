/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhpmv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhpmv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

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

test( 'zhpmv is a function', function t() {
	assert.strictEqual( typeof zhpmv, 'function' );
});

test( 'zhpmv: upper_basic (UPLO=upper, N=3, alpha=(1,0), beta=(0,0))', function t() {
	var tc = findCase( 'upper_basic' );
	// Packed upper: A(1,1)=2, A(1,2)=1+i, A(2,2)=4, A(1,3)=3-2i, A(2,3)=2+i, A(3,3)=5
	var AP = new Complex128Array( [ 2, 0, 1, 1, 4, 0, 3, -2, 2, 1, 5, 0 ] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
	var y = new Complex128Array( 3 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );

	zhpmv( 'upper', 3, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), tc.y, 1e-14, 'y' );
});

test( 'zhpmv: lower_basic (UPLO=lower, N=3, alpha=(1,0), beta=(0,0))', function t() {
	var tc = findCase( 'lower_basic' );
	// Packed lower: A(1,1)=2, A(2,1)=1-i, A(3,1)=3+2i, A(2,2)=4, A(3,2)=2-i, A(3,3)=5
	var AP = new Complex128Array( [ 2, 0, 1, -1, 3, 2, 4, 0, 2, -1, 5, 0 ] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
	var y = new Complex128Array( 3 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );

	zhpmv( 'lower', 3, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), tc.y, 1e-14, 'y' );
});

test( 'zhpmv: complex_alpha_beta (UPLO=upper, alpha=(2,1), beta=(0.5,-0.5))', function t() {
	var tc = findCase( 'complex_alpha_beta' );
	var AP = new Complex128Array( [ 2, 0, 1, 1, 4, 0, 3, -2, 2, 1, 5, 0 ] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
	var y = new Complex128Array( [ 1, 1, 2, -1, 0.5, 0.5 ] );
	var alpha = new Complex128( 2, 1 );
	var beta = new Complex128( 0.5, -0.5 );

	zhpmv( 'upper', 3, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), tc.y, 1e-14, 'y' );
});

test( 'zhpmv: alpha_zero (alpha=(0,0), beta=(2,0)) scales y only', function t() {
	var tc = findCase( 'alpha_zero' );
	var AP = new Complex128Array( [ 2, 0, 1, 1, 4, 0, 3, -2, 2, 1, 5, 0 ] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
	var y = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 2, 0 );

	zhpmv( 'upper', 3, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), tc.y, 1e-14, 'y' );
});

test( 'zhpmv: n_zero (N=0 quick return, y unchanged)', function t() {
	var tc = findCase( 'n_zero' );
	var AP = new Complex128Array( 6 );
	var x = new Complex128Array( 3 );
	var y = new Complex128Array( [ 99, 0, 0, 0, 0, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );

	zhpmv( 'upper', 0, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ).slice( 0, 2 ), tc.y, 1e-14, 'y' );
});

test( 'zhpmv: alpha_zero_beta_zero (zeros y)', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var AP = new Complex128Array( [ 2, 0, 1, 1, 4, 0, 3, -2, 2, 1, 5, 0 ] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
	var y = new Complex128Array( [ 99, 88, 77, 66, 55, 44 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 0, 0 );

	zhpmv( 'upper', 3, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), tc.y, 1e-14, 'y' );
});

test( 'zhpmv: stride_2 (incx=2, incy=2)', function t() {
	var tc = findCase( 'stride_2' );
	var AP = new Complex128Array( [ 2, 0, 1, 1, 4, 0, 3, -2, 2, 1, 5, 0 ] );
	// x with stride 2: elements at positions 0, 2, 4
	var x = new Complex128Array( [ 1, 0.5, 0, 0, 2, -1, 0, 0, 3, 1, 0, 0 ] );
	var y = new Complex128Array( 6 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );

	zhpmv( 'upper', 3, alpha, AP, 1, 0, x, 2, 0, beta, y, 2, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), tc.y, 1e-14, 'y' );
});

test( 'zhpmv: scalar (N=1, alpha=(2,1))', function t() {
	var tc = findCase( 'scalar' );
	var AP = new Complex128Array( [ 3, 0 ] );
	var x = new Complex128Array( [ 5, 2 ] );
	var y = new Complex128Array( 1 );
	var alpha = new Complex128( 2, 1 );
	var beta = new Complex128( 0, 0 );

	zhpmv( 'upper', 1, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), tc.y, 1e-14, 'y' );
});

test( 'zhpmv: lower_nonzero_beta (UPLO=lower, beta=(0.5,0))', function t() {
	var tc = findCase( 'lower_nonzero_beta' );
	var AP = new Complex128Array( [ 2, 0, 1, -1, 3, 2, 4, 0, 2, -1, 5, 0 ] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
	var y = new Complex128Array( [ 1, 1, 2, -1, 0.5, 0.5 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0.5, 0 );

	zhpmv( 'lower', 3, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), tc.y, 1e-14, 'y' );
});

test( 'zhpmv: alpha=(1,0), beta=(1,0) quick return (no computation)', function t() {
	// When alpha=0 and beta=1, should return immediately
	var AP = new Complex128Array( [ 2, 0, 1, 1, 4, 0, 3, -2, 2, 1, 5, 0 ] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
	var y = new Complex128Array( [ 7, 8, 9, 10, 11, 12 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 1, 0 );

	zhpmv( 'upper', 3, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), [ 7, 8, 9, 10, 11, 12 ], 1e-14, 'y unchanged' );
});

test( 'zhpmv: returns y', function t() {
	var AP = new Complex128Array( [ 3, 0 ] );
	var x = new Complex128Array( [ 1, 0 ] );
	var y = new Complex128Array( 1 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );

	var result = zhpmv( 'upper', 1, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
});

test( 'zhpmv: upper with complex beta (beta=(1,1))', function t() {
	var AP = new Complex128Array( [ 2, 0, 1, 1, 4, 0, 3, -2, 2, 1, 5, 0 ] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
	// y_init = [(1,0), (0,1), (1,-1)]
	var y = new Complex128Array( [ 1, 0, 0, 1, 1, -1 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 1, 1 );

	// Manually: beta*y_init = (1+i)*(1,0)=(1,1), (1+i)*(0,1)=(-1,1), (1+i)*(1,-1)=(2,0)
	// A*x from upper_basic = (16,-1), (14.5,0.5), (20,4.5)
	// y = A*x + beta*y_init = (17,0), (13.5,1.5), (22,4.5)
	zhpmv( 'upper', 3, alpha, AP, 1, 0, x, 1, 0, beta, y, 1, 0 );
	var yv = reinterpret( y, 0 );
	assertArrayClose( Array.from( yv ), [ 17, 0, 13.5, 1.5, 22, 4.5 ], 1e-14, 'y' );
});

test( 'zhpmv: lower with stride 2', function t() {
	// Same lower packed matrix, but with strides=2
	var AP = new Complex128Array( [ 2, 0, 1, -1, 3, 2, 4, 0, 2, -1, 5, 0 ] );
	var x = new Complex128Array( [ 1, 0.5, 0, 0, 2, -1, 0, 0, 3, 1, 0, 0 ] );
	var y = new Complex128Array( 6 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );

	zhpmv( 'lower', 3, alpha, AP, 1, 0, x, 2, 0, beta, y, 2, 0 );
	var yv = reinterpret( y, 0 );
	// Same result as lower_basic but interspersed with zeros at odd positions
	// lower_basic y = [16, -1, 14.5, 0.5, 20, 4.5]
	assertArrayClose( Array.from( yv ), [ 16, -1, 0, 0, 14.5, 0.5, 0, 0, 20, 4.5, 0, 0 ], 1e-14, 'y' );
});

test( 'zhpmv: upper with offset', function t() {
	// Test offsetAP, offsetX, offsetY
	var AP = new Complex128Array( [ 0, 0, 2, 0, 1, 1, 4, 0, 3, -2, 2, 1, 5, 0 ] );
	var x = new Complex128Array( [ 0, 0, 1, 0.5, 2, -1, 3, 1 ] );
	var y = new Complex128Array( [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );

	zhpmv( 'upper', 3, alpha, AP, 1, 1, x, 1, 1, beta, y, 1, 1 );
	var yv = reinterpret( y, 0 );
	// y[1..3] should match upper_basic
	assertArrayClose( Array.from( yv ).slice( 2, 8 ), [ 16, -1, 14.5, 0.5, 20, 4.5 ], 1e-14, 'y with offset' );
});
