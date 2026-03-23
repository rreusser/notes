

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zher = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zher.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zher: upper_basic (UPLO=U, N=2, alpha=1)', function t() {
	var tc = findCase( 'upper_basic' );
	// A = [2 (1+i); . 3] (upper, column-major, LDA=2)
	var A = new Complex128Array( [
		2, 0, 0, 0,
		1, 1, 3, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	var result = zher( 'U', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: lower_basic (UPLO=L, N=2, alpha=1)', function t() {
	var tc = findCase( 'lower_basic' );
	// A = [2 .; (1-i) 3] (lower, column-major, LDA=2)
	var A = new Complex128Array( [
		2, 0, 1, -1,
		0, 0, 3, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	var result = zher( 'L', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: n_zero (N=0 quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( [ 99, 88 ] );
	var x = new Complex128Array( 0 );
	var result = zher( 'U', 0, 1.0, x, 1, 0, A, 1, 1, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: alpha_zero (alpha=0 quick return)', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Complex128Array( [ 99, 88 ] );
	var x = new Complex128Array( [ 1, 2 ] );
	var result = zher( 'U', 1, 0.0, x, 1, 0, A, 1, 1, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: n_one (N=1)', function t() {
	var tc = findCase( 'n_one' );
	var A = new Complex128Array( [ 5, 0 ] );
	var x = new Complex128Array( [ 2, 3 ] );
	var result = zher( 'U', 1, 1.0, x, 1, 0, A, 1, 1, 0 );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: upper_stride (UPLO=U, N=3, incx=2, alpha=2)', function t() {
	var tc = findCase( 'upper_stride' );
	// A = [2 (1+i) (2-i); . 3 (1+2i); . . 4] (upper, column-major, LDA=3)
	var A = new Complex128Array( [
		2, 0, 0, 0, 0, 0,
		1, 1, 3, 0, 0, 0,
		2, -1, 1, 2, 4, 0
	] );
	// x with stride 2: x(1)=(1,0), x(3)=(0,1), x(5)=(1,1)
	var x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	var result = zher( 'U', 3, 2.0, x, 2, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: lower_stride (UPLO=L, N=3, incx=2, alpha=2)', function t() {
	var tc = findCase( 'lower_stride' );
	// A = [2 . .; (1-i) 3 .; (2+i) (1-2i) 4] (lower, column-major, LDA=3)
	var A = new Complex128Array( [
		2, 0, 1, -1, 2, 1,
		0, 0, 3, 0, 1, -2,
		0, 0, 0, 0, 4, 0
	] );
	// x with stride 2: x(1)=(1,0), x(3)=(0,1), x(5)=(1,1)
	var x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	var result = zher( 'L', 3, 2.0, x, 2, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: upper_alpha2 (UPLO=U, N=3, alpha=2)', function t() {
	var tc = findCase( 'upper_alpha2' );
	var A = new Complex128Array( [
		1, 0, 0, 0, 0, 0,
		2, 1, 3, 0, 0, 0,
		0, -1, 1, 1, 5, 0
	] );
	var x = new Complex128Array( [ 1, 1, 2, 0, 0, 3 ] );
	var result = zher( 'U', 3, 2.0, x, 1, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: upper_zeros (x has zeros, tests skip branch)', function t() {
	var tc = findCase( 'upper_zeros' );
	var A = new Complex128Array( [
		1, 0.5, 0, 0,
		2, 1, 3, 0.7
	] );
	var x = new Complex128Array( [ 0, 0, 1, 0 ] );
	var result = zher( 'U', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: lower_zeros (x has zeros, tests skip branch)', function t() {
	var tc = findCase( 'lower_zeros' );
	var A = new Complex128Array( [
		1, 0.5, 2, -1,
		0, 0, 3, 0.7
	] );
	var x = new Complex128Array( [ 0, 0, 1, 0 ] );
	var result = zher( 'L', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: negative_stride (UPLO=U, N=2, incx=-1)', function t() {
	var tc = findCase( 'negative_stride' );
	var A = new Complex128Array( [
		2, 0, 0, 0,
		1, 1, 3, 0
	] );
	// x = [(0,1), (1,0)], incx=-1 so effectively [(1,0), (0,1)]
	var x = new Complex128Array( [ 0, 1, 1, 0 ] );
	// Negative stride: strideX=-1, offsetX=1 (start from end)
	var result = zher( 'U', 2, 1.0, x, -1, 1, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});
