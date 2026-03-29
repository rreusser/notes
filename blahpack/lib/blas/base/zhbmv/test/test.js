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
var zhbmv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhbmv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zhbmv: main export is a function', function t() {
	assert.strictEqual( typeof zhbmv, 'function' );
});

test( 'zhbmv: upper_basic (UPLO=U, N=4, K=2, alpha=(1,0), beta=(0,0))', function t() {
	var tc = findCase( 'upper_basic' );
	// 4x4 Hermitian band matrix, K=2, upper band storage (LDA=3)
	// Column-major band storage: each column has 3 entries (K+1 rows)
	// Col 1: [*, *, d1]           => [0,0, 0,0, 2,0]
	// Col 2: [*, a12, d2]         => [0,0, 1,1, 4,0]
	// Col 3: [a13, a23, d3]       => [3,-2, 2,1, 5,0]
	// Col 4: [a24, a34, d4]       => [0.5,-1, 1.5,0.5, 3,0]
	var A = new Complex128Array( [
		0, 0, 0, 0, 2, 0,
		0, 0, 1, 1, 4, 0,
		3, -2, 2, 1, 5, 0,
		0.5, -1, 1.5, 0.5, 3, 0
	] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 0 ] );
	var y = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhbmv( 'upper', 4, 2, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: lower_basic (UPLO=L, N=4, K=2, alpha=(1,0), beta=(0,0))', function t() {
	var tc = findCase( 'lower_basic' );
	// Same Hermitian matrix stored in lower band (LDA=3)
	// Col 1: [d1, conj(a12), conj(a13)] => [2,0, 1,-1, 3,2]
	// Col 2: [d2, conj(a23), conj(a24)] => [4,0, 2,-1, 0.5,1]
	// Col 3: [d3, conj(a34), *]         => [5,0, 1.5,-0.5, 0,0]
	// Col 4: [d4, *, *]                 => [3,0, 0,0, 0,0]
	var A = new Complex128Array( [
		2, 0, 1, -1, 3, 2,
		4, 0, 2, -1, 0.5, 1,
		5, 0, 1.5, -0.5, 0, 0,
		3, 0, 0, 0, 0, 0
	] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 0 ] );
	var y = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhbmv( 'lower', 4, 2, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: complex_alpha_beta (alpha=(2,1), beta=(0.5,-0.5))', function t() {
	var tc = findCase( 'complex_alpha_beta' );
	// Same upper band matrix as test 1
	var A = new Complex128Array( [
		0, 0, 0, 0, 2, 0,
		0, 0, 1, 1, 4, 0,
		3, -2, 2, 1, 5, 0,
		0.5, -1, 1.5, 0.5, 3, 0
	] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 0 ] );
	var y = new Complex128Array( [ 1, 1, 2, -1, 0.5, 0.5, 3, 0 ] );
	var alpha = new Complex128( 2, 1 );
	var beta = new Complex128( 0.5, -0.5 );
	var result = zhbmv( 'upper', 4, 2, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: alpha_zero (alpha=0, beta=(2,0) scales y only)', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Complex128Array( [
		0, 0, 0, 0, 2, 0,
		0, 0, 1, 1, 4, 0,
		3, -2, 2, 1, 5, 0,
		0.5, -1, 1.5, 0.5, 3, 0
	] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 0 ] );
	var y = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 2, 0 );
	var result = zhbmv( 'upper', 4, 2, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: n_zero (N=0 quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 0 );
	var x = new Complex128Array( 0 );
	var y = new Complex128Array( [ 99, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhbmv( 'upper', 0, 2, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: alpha_zero_beta_zero (alpha=0, beta=0 zeroes y)', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var A = new Complex128Array( [
		0, 0, 0, 0, 2, 0,
		0, 0, 1, 1, 4, 0,
		3, -2, 2, 1, 5, 0,
		0.5, -1, 1.5, 0.5, 3, 0
	] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 0 ] );
	var y = new Complex128Array( [ 99, 88, 77, 66, 55, 44, 33, 22 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhbmv( 'upper', 4, 2, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: stride_2 (non-unit strides)', function t() {
	var tc = findCase( 'stride_2' );
	// Same upper band matrix, but x and y have stride 2
	var A = new Complex128Array( [
		0, 0, 0, 0, 2, 0,
		0, 0, 1, 1, 4, 0,
		3, -2, 2, 1, 5, 0,
		0.5, -1, 1.5, 0.5, 3, 0
	] );
	// x with stride 2: values at complex indices 0, 2, 4, 6
	var x = new Complex128Array( [
		1, 0.5, 0, 0, 2, -1, 0, 0, 3, 1, 0, 0, 4, 0, 0, 0
	] );
	var y = new Complex128Array( 8 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhbmv( 'upper', 4, 2, alpha, A, 1, 3, 0, x, 2, 0, beta, y, 2, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: scalar (1x1, K=0, alpha=(2,1))', function t() {
	var tc = findCase( 'scalar' );
	// A = [5+0i], x = [3+2i], alpha = (2,1), beta = (0,0)
	var A = new Complex128Array( [ 5, 0 ] );
	var x = new Complex128Array( [ 3, 2 ] );
	var y = new Complex128Array( 1 );
	var alpha = new Complex128( 2, 1 );
	var beta = new Complex128( 0, 0 );
	var result = zhbmv( 'upper', 1, 0, alpha, A, 1, 1, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: alpha=0, beta=1 quick return (y unchanged)', function t() {
	var A = new Complex128Array( [
		0, 0, 0, 0, 2, 0,
		0, 0, 1, 1, 4, 0,
		3, -2, 2, 1, 5, 0,
		0.5, -1, 1.5, 0.5, 3, 0
	] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 0 ] );
	var y = new Complex128Array( [ 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 1, 0 );
	var result = zhbmv( 'upper', 4, 2, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	var yv = reinterpret( y, 0 );
	assert.strictEqual( yv[ 0 ], 5 );
	assert.strictEqual( yv[ 1 ], 6 );
	assert.strictEqual( yv[ 2 ], 7 );
	assert.strictEqual( yv[ 3 ], 8 );
	assert.strictEqual( yv[ 4 ], 9 );
	assert.strictEqual( yv[ 5 ], 10 );
	assert.strictEqual( yv[ 6 ], 11 );
	assert.strictEqual( yv[ 7 ], 12 );
});

test( 'zhbmv: lower with non-unit strides', function t() {
	// Use the same Hermitian matrix in lower band storage with stride 2
	var A = new Complex128Array( [
		2, 0, 1, -1, 3, 2,
		4, 0, 2, -1, 0.5, 1,
		5, 0, 1.5, -0.5, 0, 0,
		3, 0, 0, 0, 0, 0
	] );
	// x with stride 2
	var x = new Complex128Array( [
		1, 0.5, 0, 0, 2, -1, 0, 0, 3, 1, 0, 0, 4, 0, 0, 0
	] );
	var y = new Complex128Array( 8 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhbmv( 'lower', 4, 2, alpha, A, 1, 3, 0, x, 2, 0, beta, y, 2, 0 );
	assert.strictEqual( result, y );
	// Lower result should match upper result since it is the same Hermitian matrix
	var tc = findCase( 'stride_2' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: lower with complex alpha and beta', function t() {
	// Same Hermitian matrix in lower band storage with complex alpha/beta
	var A = new Complex128Array( [
		2, 0, 1, -1, 3, 2,
		4, 0, 2, -1, 0.5, 1,
		5, 0, 1.5, -0.5, 0, 0,
		3, 0, 0, 0, 0, 0
	] );
	var x = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 0 ] );
	var y = new Complex128Array( [ 1, 1, 2, -1, 0.5, 0.5, 3, 0 ] );
	var alpha = new Complex128( 2, 1 );
	var beta = new Complex128( 0.5, -0.5 );
	var result = zhbmv( 'lower', 4, 2, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	// Should match the upper complex_alpha_beta result
	var tc = findCase( 'complex_alpha_beta' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhbmv: K=1 bandwidth (tridiagonal-like)', function t() {
	// 3x3 Hermitian with K=1: only diagonal and 1 super-diagonal
	// Full: [ 2     (1+i)    0   ]
	//       [ (1-i)   3    (2-i) ]
	//       [  0    (2+i)    4   ]
	// Upper band (LDA=2):
	// Row 0 (1st superdiag): *     (1+i)  (2-i)
	// Row 1 (diagonal):      2      3      4
	var A = new Complex128Array( [
		0, 0, 2, 0,
		1, 1, 3, 0,
		2, -1, 4, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 1, 1, -1 ] );
	var y = new Complex128Array( 3 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	zhbmv( 'upper', 3, 1, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );

	// Manual: y = A * x
	// y[0] = 2*(1,0) + (1+i)*(0,1) = (2,0) + (-1,1) = (1,1)
	// y[1] = (1-i)*(1,0) + 3*(0,1) + (2-i)*(1,-1) = (1,-1) + (0,3) + (1,-3) = (2,-1)
	// y[2] = (2+i)*(0,1) + 4*(1,-1) = (-1,2) + (4,-4) = (3,-2)
	var yv = reinterpret( y, 0 );
	assertClose( yv[ 0 ], 1, 1e-14, 'y[0].re' );
	assertClose( yv[ 1 ], 1, 1e-14, 'y[0].im' );
	assertClose( yv[ 2 ], 2, 1e-14, 'y[1].re' );
	assertClose( yv[ 3 ], -1, 1e-14, 'y[1].im' );
	assertClose( yv[ 4 ], 3, 1e-14, 'y[2].re' );
	assertClose( yv[ 5 ], -2, 1e-14, 'y[2].im' );
});

test( 'zhbmv: lower K=1 bandwidth (tridiagonal-like)', function t() {
	// Same matrix in lower band storage (LDA=2):
	// Row 0 (diagonal):      2      3      4
	// Row 1 (1st subdiag):  (1-i)  (2+i)   *
	var A = new Complex128Array( [
		2, 0, 1, -1,
		3, 0, 2, 1,
		4, 0, 0, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 1, 1, -1 ] );
	var y = new Complex128Array( 3 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	zhbmv( 'lower', 3, 1, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );

	var yv = reinterpret( y, 0 );
	assertClose( yv[ 0 ], 1, 1e-14, 'y[0].re' );
	assertClose( yv[ 1 ], 1, 1e-14, 'y[0].im' );
	assertClose( yv[ 2 ], 2, 1e-14, 'y[1].re' );
	assertClose( yv[ 3 ], -1, 1e-14, 'y[1].im' );
	assertClose( yv[ 4 ], 3, 1e-14, 'y[2].re' );
	assertClose( yv[ 5 ], -2, 1e-14, 'y[2].im' );
});

test( 'zhbmv: beta scaling with non-trivial imaginary part', function t() {
	// Test that complex beta scaling works correctly
	var A = new Complex128Array( [ 3, 0 ] );  // 1x1, K=0
	var x = new Complex128Array( [ 1, 0 ] );
	var y = new Complex128Array( [ 2, 3 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 1 );  // purely imaginary beta
	zhbmv( 'upper', 1, 0, alpha, A, 1, 1, 0, x, 1, 0, beta, y, 1, 0 );
	// y = alpha*A*x + beta*y = 1*(3+0i)*(1+0i) + i*(2+3i) = (3,0) + (-3,2) = (0,2)
	var yv = reinterpret( y, 0 );
	assertClose( yv[ 0 ], 0, 1e-14, 'y[0].re' );
	assertClose( yv[ 1 ], 2, 1e-14, 'y[0].im' );
});
