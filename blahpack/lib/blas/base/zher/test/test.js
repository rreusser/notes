/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zher = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


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
	var result = zher( 'upper', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	var tc = findCase( 'upper_basic' );
	var A = new Complex128Array([
		2,
		0,
		0,
		0,
		1,
		1,
		3,
		0
	]);
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: lower_basic (UPLO=L, N=2, alpha=1)', function t() {
	var result = zher( 'lower', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	var tc = findCase( 'lower_basic' );
	var A = new Complex128Array([
		2,
		0,
		1,
		-1,
		0,
		0,
		3,
		0
	]);
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: n_zero (N=0 quick return)', function t() {
	var result = zher( 'upper', 0, 1.0, x, 1, 0, A, 1, 1, 0 );
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( [ 99, 88 ] );
	var x = new Complex128Array( 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: alpha_zero (alpha=0 quick return)', function t() {
	var result = zher( 'upper', 1, 0.0, x, 1, 0, A, 1, 1, 0 );
	var tc = findCase( 'alpha_zero' );
	var A = new Complex128Array( [ 99, 88 ] );
	var x = new Complex128Array( [ 1, 2 ] );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: n_one (N=1)', function t() {
	var result = zher( 'upper', 1, 1.0, x, 1, 0, A, 1, 1, 0 );
	var tc = findCase( 'n_one' );
	var A = new Complex128Array( [ 5, 0 ] );
	var x = new Complex128Array( [ 2, 3 ] );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: upper_stride (UPLO=U, N=3, incx=2, alpha=2)', function t() {
	var result = zher( 'upper', 3, 2.0, x, 2, 0, A, 1, 3, 0 );
	var tc = findCase( 'upper_stride' );
	var A = new Complex128Array([
		2,
		0,
		0,
		0,
		0,
		0,
		1,
		1,
		3,
		0,
		0,
		0,
		2,
		-1,
		1,
		2,
		4,
		0
	]);
	var x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: lower_stride (UPLO=L, N=3, incx=2, alpha=2)', function t() {
	var result = zher( 'lower', 3, 2.0, x, 2, 0, A, 1, 3, 0 );
	var tc = findCase( 'lower_stride' );
	var A = new Complex128Array([
		2,
		0,
		1,
		-1,
		2,
		1,
		0,
		0,
		3,
		0,
		1,
		-2,
		0,
		0,
		0,
		0,
		4,
		0
	]);
	var x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: upper_alpha2 (UPLO=U, N=3, alpha=2)', function t() {
	var result = zher( 'upper', 3, 2.0, x, 1, 0, A, 1, 3, 0 );
	var tc = findCase( 'upper_alpha2' );
	var A = new Complex128Array([
		1,
		0,
		0,
		0,
		0,
		0,
		2,
		1,
		3,
		0,
		0,
		0,
		0,
		-1,
		1,
		1,
		5,
		0
	]);
	var x = new Complex128Array( [ 1, 1, 2, 0, 0, 3 ] );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: upper_zeros (x has zeros, tests skip branch)', function t() {
	var result = zher( 'upper', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	var tc = findCase( 'upper_zeros' );
	var A = new Complex128Array([
		1,
		0.5,
		0,
		0,
		2,
		1,
		3,
		0.7
	]);
	var x = new Complex128Array( [ 0, 0, 1, 0 ] );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: lower_zeros (x has zeros, tests skip branch)', function t() {
	var result = zher( 'lower', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	var tc = findCase( 'lower_zeros' );
	var A = new Complex128Array([
		1,
		0.5,
		2,
		-1,
		0,
		0,
		3,
		0.7
	]);
	var x = new Complex128Array( [ 0, 0, 1, 0 ] );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: negative_stride (UPLO=U, N=2, incx=-1)', function t() {
	var result = zher( 'upper', 2, 1.0, x, -1, 1, A, 1, 2, 0 );
	var tc = findCase( 'negative_stride' );
	var A = new Complex128Array([
		2,
		0,
		0,
		0,
		1,
		1,
		3,
		0
	]);
	var x = new Complex128Array( [ 0, 1, 1, 0 ] );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

// ndarray validation tests

test( 'zher: ndarray throws TypeError for invalid uplo', function t() {
	assert.throws( function () {
		ndarray( 'invalid', 2, 1.0, new Complex128Array( 2 ), 1, 0, new Complex128Array( 4 ), 1, 2, 0 );
	}, TypeError );
});

test( 'zher: ndarray throws RangeError for negative N', function t() {
	assert.throws( function () {
		ndarray( 'upper', -1, 1.0, new Complex128Array( 2 ), 1, 0, new Complex128Array( 4 ), 1, 2, 0 );
	}, RangeError );
});

test( 'zher: ndarray throws RangeError for zero strideX', function t() {
	assert.throws( function () {
		ndarray( 'upper', 2, 1.0, new Complex128Array( 2 ), 0, 0, new Complex128Array( 4 ), 1, 2, 0 );
	}, RangeError );
});
