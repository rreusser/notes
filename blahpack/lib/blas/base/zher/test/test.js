/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

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

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zher.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

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

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'zher: upper_basic (UPLO=U, N=2, alpha=1)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'upper_basic' );
	A = new Complex128Array([
		2,
		0,
		0,
		0,
		1,
		1,
		3,
		0
	]);
	x = new Complex128Array( [ 1, 0, 0, 1 ] );
	result = zher( 'upper', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: lower_basic (UPLO=L, N=2, alpha=1)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'lower_basic' );
	A = new Complex128Array([
		2,
		0,
		1,
		-1,
		0,
		0,
		3,
		0
	]);
	x = new Complex128Array( [ 1, 0, 0, 1 ] );
	result = zher( 'lower', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: n_zero (N=0 quick return)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'n_zero' );
	A = new Complex128Array( [ 99, 88 ] );
	x = new Complex128Array( 0 );
	result = zher( 'upper', 0, 1.0, x, 1, 0, A, 1, 1, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: alpha_zero (alpha=0 quick return)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'alpha_zero' );
	A = new Complex128Array( [ 99, 88 ] );
	x = new Complex128Array( [ 1, 2 ] );
	result = zher( 'upper', 1, 0.0, x, 1, 0, A, 1, 1, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: n_one (N=1)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'n_one' );
	A = new Complex128Array( [ 5, 0 ] );
	x = new Complex128Array( [ 2, 3 ] );
	result = zher( 'upper', 1, 1.0, x, 1, 0, A, 1, 1, 0 );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: upper_stride (UPLO=U, N=3, incx=2, alpha=2)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'upper_stride' );
	A = new Complex128Array([
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
	x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	result = zher( 'upper', 3, 2.0, x, 2, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: lower_stride (UPLO=L, N=3, incx=2, alpha=2)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'lower_stride' );
	A = new Complex128Array([
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
	x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	result = zher( 'lower', 3, 2.0, x, 2, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: upper_alpha2 (UPLO=U, N=3, alpha=2)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'upper_alpha2' );
	A = new Complex128Array([
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
	x = new Complex128Array( [ 1, 1, 2, 0, 0, 3 ] );
	result = zher( 'upper', 3, 2.0, x, 1, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: upper_zeros (x has zeros, tests skip branch)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'upper_zeros' );
	A = new Complex128Array([
		1,
		0.5,
		0,
		0,
		2,
		1,
		3,
		0.7
	]);
	x = new Complex128Array( [ 0, 0, 1, 0 ] );
	result = zher( 'upper', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: lower_zeros (x has zeros, tests skip branch)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'lower_zeros' );
	A = new Complex128Array([
		1,
		0.5,
		2,
		-1,
		0,
		0,
		3,
		0.7
	]);
	x = new Complex128Array( [ 0, 0, 1, 0 ] );
	result = zher( 'lower', 2, 1.0, x, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher: negative_stride (UPLO=U, N=2, incx=-1)', function t() {
	var result;
	var tc;
	var A;
	var x;

	tc = findCase( 'negative_stride' );
	A = new Complex128Array([
		2,
		0,
		0,
		0,
		1,
		1,
		3,
		0
	]);
	x = new Complex128Array( [ 0, 1, 1, 0 ] );
	result = zher( 'upper', 2, 1.0, x, -1, 1, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

// ndarray validation tests

test( 'zher: ndarray throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ndarray( 'invalid', 2, 1.0, new Complex128Array( 2 ), 1, 0, new Complex128Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'zher: ndarray throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ndarray( 'upper', -1, 1.0, new Complex128Array( 2 ), 1, 0, new Complex128Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'zher: ndarray throws RangeError for zero strideX', function t() {
	assert.throws( function throws() {
		ndarray( 'upper', 2, 1.0, new Complex128Array( 2 ), 0, 0, new Complex128Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, RangeError );
});
