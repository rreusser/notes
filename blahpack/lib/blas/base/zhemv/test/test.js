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
var zhemv = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhemv.jsonl' ), 'utf8' ).trim().split( '\n' );
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
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zhemv: upper_basic (UPLO=U, N=3, alpha=(1,0), beta=(0,0))', function t() {
	var tc = findCase( 'upper_basic' );
	// Hermitian 3x3 (upper stored, column-major):
	// A = [ 2    (1+i)  (2-i) ]
	//     [ .     3     (1+2i)]
	//     [ .     .      4    ]
	var A = new Complex128Array( [
		2, 0, 0, 0, 0, 0,
		1, 1, 3, 0, 0, 0,
		2, -1, 1, 2, 4, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 1, 1, 1 ] );
	var y = new Complex128Array( 3 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhemv( 'upper', 3, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhemv: lower_basic (UPLO=L, N=3, alpha=(1,0), beta=(0,0))', function t() {
	var tc = findCase( 'lower_basic' );
	// Same Hermitian matrix, lower stored (column-major):
	// A = [ 2      .      . ]
	//     [(1-i)   3      . ]
	//     [(2+i)  (1-2i)  4 ]
	var A = new Complex128Array( [
		2, 0, 1, -1, 2, 1,
		0, 0, 3, 0, 1, -2,
		0, 0, 0, 0, 4, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 1, 1, 1 ] );
	var y = new Complex128Array( 3 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhemv( 'lower', 3, alpha, A, 1, 3, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhemv: n_zero (N=0 quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 0 );
	var x = new Complex128Array( 0 );
	var y = new Complex128Array( [ 99, 88 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhemv( 'upper', 0, alpha, A, 1, 1, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhemv: alpha_zero_beta_one (alpha=0, beta=1 quick return)', function t() {
	var tc = findCase( 'alpha_zero_beta_one' );
	var A = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 0 ] );
	var y = new Complex128Array( [ 5, 6, 7, 8 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 1, 0 );
	var result = zhemv( 'upper', 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhemv: alpha_zero_beta_scale (alpha=0, beta=(2,1) — scale y only)', function t() {
	var tc = findCase( 'alpha_zero_beta_scale' );
	var A = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 0 ] );
	var y = new Complex128Array( [ 1, 0, 0, 1 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 2, 1 );
	var result = zhemv( 'upper', 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhemv: n_one (N=1)', function t() {
	var tc = findCase( 'n_one' );
	// A(1,1)=5 (real), x=(2,3), y=(1,1), alpha=(1,0), beta=(1,0)
	var A = new Complex128Array( [ 5, 0 ] );
	var x = new Complex128Array( [ 2, 3 ] );
	var y = new Complex128Array( [ 1, 1 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 1, 0 );
	var result = zhemv( 'upper', 1, alpha, A, 1, 1, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhemv: upper_stride (UPLO=U, incx=2, incy=2)', function t() {
	var tc = findCase( 'upper_stride' );
	// Same Hermitian 3x3 upper
	var A = new Complex128Array( [
		2, 0, 0, 0, 0, 0,
		1, 1, 3, 0, 0, 0,
		2, -1, 1, 2, 4, 0
	] );
	// x with stride=2: elements at 0, 2, 4 (complex indices)
	var x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	var y = new Complex128Array( 5 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhemv( 'upper', 3, alpha, A, 1, 3, 0, x, 2, 0, beta, y, 2, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhemv: lower_stride (UPLO=L, incx=2, incy=2)', function t() {
	var tc = findCase( 'lower_stride' );
	// Same Hermitian 3x3 lower
	var A = new Complex128Array( [
		2, 0, 1, -1, 2, 1,
		0, 0, 3, 0, 1, -2,
		0, 0, 0, 0, 4, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	var y = new Complex128Array( 5 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhemv( 'lower', 3, alpha, A, 1, 3, 0, x, 2, 0, beta, y, 2, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhemv: complex_alpha_beta (alpha=(2,1), beta=(1,-1))', function t() {
	var tc = findCase( 'complex_alpha_beta' );
	// Fortran test 9: 2x2 Hermitian upper, LDA=2
	// A(1,1)=(2,0), A(1,2)=(0,0), A(2,2)=(1,1) [only real part 1 used]
	// UPLO='U', A = [2 0; 0 1]
	var A = new Complex128Array( [
		2, 0, 0, 0,
		0, 0, 1, 1
	] );
	var x = new Complex128Array( [ 1, 0, 1, 0 ] );
	var y = new Complex128Array( [ 1, 0, 0, 1 ] );
	var alpha = new Complex128( 2, 1 );
	var beta = new Complex128( 1, -1 );
	var result = zhemv( 'upper', 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zhemv: beta_zero (beta=0 zeroes y first)', function t() {
	var tc = findCase( 'beta_zero' );
	// 2x2 Hermitian upper: A = [1 (1+i); . 2]
	var A = new Complex128Array( [
		1, 0, 0, 0,
		1, 1, 2, 0
	] );
	var x = new Complex128Array( [ 1, 0, 1, 0 ] );
	var y = new Complex128Array( [ 99, 99, 99, 99 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = zhemv( 'upper', 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

// ndarray validation tests

test( 'zhemv: ndarray throws TypeError for invalid uplo', function t() {
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	assert.throws( function() {
		ndarray( 'invalid', 2, alpha, new Complex128Array( 4 ), 1, 2, 0, new Complex128Array( 2 ), 1, 0, beta, new Complex128Array( 2 ), 1, 0 );
	}, TypeError );
});

test( 'zhemv: ndarray throws RangeError for negative N', function t() {
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	assert.throws( function() {
		ndarray( 'upper', -1, alpha, new Complex128Array( 4 ), 1, 2, 0, new Complex128Array( 2 ), 1, 0, beta, new Complex128Array( 2 ), 1, 0 );
	}, RangeError );
});

test( 'zhemv: ndarray throws RangeError for zero strideX', function t() {
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	assert.throws( function() {
		ndarray( 'upper', 2, alpha, new Complex128Array( 4 ), 1, 2, 0, new Complex128Array( 2 ), 0, 0, beta, new Complex128Array( 2 ), 1, 0 );
	}, RangeError );
});

test( 'zhemv: ndarray throws RangeError for zero strideY', function t() {
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	assert.throws( function() {
		ndarray( 'upper', 2, alpha, new Complex128Array( 4 ), 1, 2, 0, new Complex128Array( 2 ), 1, 0, beta, new Complex128Array( 2 ), 0, 0 );
	}, RangeError );
});
