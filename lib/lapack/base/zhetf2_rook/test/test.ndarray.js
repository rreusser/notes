/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len, max-lines, node/no-sync */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zhetf2Rook = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhetf2_rook.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


// FUNCTIONS //

/**
* Find a fixture case by name.
*
* @private
* @param {string} name - case name
* @returns {Object} fixture case
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	});
}

/**
* Assert that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Assert that two arrays are element-wise approximately equal.
*
* @private
* @param {Object} actual - actual array
* @param {Array} expected - expected array
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
* Extract an n-by-n submatrix from a Fortran LDA-strided complex output array.
*
* @private
* @param {Array} data - interleaved re/im values with leading dimension `lda`
* @param {integer} n - matrix order
* @param {integer} lda - Fortran leading dimension
* @returns {Array} interleaved re/im for the n-by-n submatrix
*/
function extractSubmatrix( data, n, lda ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i < n; i++ ) {
			out.push( data[ (j * lda * 2) + (i * 2) ] );
			out.push( data[ (j * lda * 2) + (i * 2) + 1 ] );
		}
	}
	return out;
}

/**
* Convert Fortran 1-based IPIV to 0-based JS IPIV.
*
* @private
* @param {Array} fipiv - Fortran IPIV values
* @returns {Array} JS IPIV values
*/
function convertIPIV( fipiv ) {
	var out = [];
	var i;
	for ( i = 0; i < fipiv.length; i++ ) {
		if ( fipiv[ i ] > 0 ) {
			out.push( fipiv[ i ] - 1 );
		} else {
			out.push( fipiv[ i ] );
		}
	}
	return out;
}

/**
* Convert a typed array to a plain array.
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

test( 'zhetf2Rook: upper_4x4', function t() {
	var expected;
	var IPIV;
	var info;
	var Av;
	var tc;
	var A;
	var n;

	tc = findCase( 'upper_4x4' );
	n = 4;
	A = new Complex128Array([
		4,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		1,
		2,
		5,
		0,
		0,
		0,
		0,
		0,
		3,
		-1,
		2,
		1,
		7,
		0,
		0,
		0,
		0.5,
		0.5,
		1,
		-2,
		3,
		0,
		6,
		0
	]);
	IPIV = new Int32Array( n );
	info = zhetf2Rook( 'upper', n, A, 1, n, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	expected = extractSubmatrix( tc.A, n, 6 );
	Av = reinterpret( A, 0 );
	assertArrayClose( toArray( Av ), expected, 1e-12, 'A' );
	assert.deepEqual( toArray( IPIV ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'zhetf2Rook: lower_4x4', function t() {
	var expected;
	var IPIV;
	var info;
	var Av;
	var tc;
	var A;
	var n;

	tc = findCase( 'lower_4x4' );
	n = 4;
	A = new Complex128Array([
		4,
		0,
		1,
		-2,
		3,
		1,
		0.5,
		-0.5,
		0,
		0,
		5,
		0,
		2,
		-1,
		1,
		2,
		0,
		0,
		0,
		0,
		7,
		0,
		3,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		6,
		0
	]);
	IPIV = new Int32Array( n );
	info = zhetf2Rook( 'lower', n, A, 1, n, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	expected = extractSubmatrix( tc.A, n, 6 );
	Av = reinterpret( A, 0 );
	assertArrayClose( toArray( Av ), expected, 1e-12, 'A' );
	assert.deepEqual( toArray( IPIV ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'zhetf2Rook: n0 quick return', function t() {
	var IPIV;
	var info;
	var A;

	A = new Complex128Array( 0 );
	IPIV = new Int32Array( 0 );
	info = zhetf2Rook( 'upper', 0, A, 1, 1, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'zhetf2Rook: n1', function t() {
	var IPIV;
	var info;
	var Av;
	var tc;
	var A;

	tc = findCase( 'n1' );
	A = new Complex128Array([
		3.0,
		0.0
	]);
	IPIV = new Int32Array( 1 );
	info = zhetf2Rook( 'upper', 1, A, 1, 1, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	Av = reinterpret( A, 0 );
	assertArrayClose( toArray( Av ), tc.A, 1e-14, 'A' );
	assert.deepEqual( toArray( IPIV ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'zhetf2Rook: singular_upper', function t() {
	var expected;
	var IPIV;
	var info;
	var Av;
	var tc;
	var A;
	var n;

	tc = findCase( 'singular_upper' );
	n = 3;
	A = new Complex128Array([
		0,
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
		0,
		1,
		1,
		2,
		0
	]);
	IPIV = new Int32Array( n );
	info = zhetf2Rook( 'upper', n, A, 1, n, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	expected = extractSubmatrix( tc.A, n, 6 );
	Av = reinterpret( A, 0 );
	assertArrayClose( toArray( Av ), expected, 1e-12, 'A' );
	assert.deepEqual( toArray( IPIV ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'zhetf2Rook: lower_6x6', function t() {
	var expected;
	var IPIV;
	var info;
	var Av;
	var tc;
	var A;
	var n;

	tc = findCase( 'lower_6x6' );
	n = 6;
	A = new Complex128Array([
		0.01,
		0,
		5,
		-1,
		1,
		1,
		0.5,
		-0.5,
		2,
		0,
		1,
		-1,
		0,
		0,
		0.02,
		0,
		2,
		-1,
		1,
		1,
		1.5,
		-0.5,
		0,
		-3,
		0,
		0,
		0,
		0,
		8,
		0,
		3,
		0,
		0,
		2,
		1,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		7,
		0,
		1,
		0.5,
		2,
		-2,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		6,
		0,
		0.5,
		1,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		5,
		0
	]);
	IPIV = new Int32Array( n );
	info = zhetf2Rook( 'lower', n, A, 1, n, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	expected = extractSubmatrix( tc.A, n, 6 );
	Av = reinterpret( A, 0 );
	assertArrayClose( toArray( Av ), expected, 1e-11, 'A' );
	assert.deepEqual( toArray( IPIV ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'zhetf2Rook: upper_6x6', function t() {
	var expected;
	var IPIV;
	var info;
	var Av;
	var tc;
	var A;
	var n;

	tc = findCase( 'upper_6x6' );
	n = 6;
	A = new Complex128Array([
		0.01,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		5,
		1,
		0.02,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		1,
		-1,
		2,
		1,
		8,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0.5,
		0.5,
		1,
		-1,
		3,
		0,
		7,
		0,
		0,
		0,
		0,
		0,
		2,
		0,
		1.5,
		0.5,
		0,
		-2,
		1,
		-0.5,
		6,
		0,
		0,
		0,
		1,
		1,
		0,
		3,
		1,
		0,
		2,
		2,
		0.5,
		-1,
		5,
		0
	]);
	IPIV = new Int32Array( n );
	info = zhetf2Rook( 'upper', n, A, 1, n, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	expected = extractSubmatrix( tc.A, n, 6 );
	Av = reinterpret( A, 0 );
	assertArrayClose( toArray( Av ), expected, 1e-11, 'A' );
	assert.deepEqual( toArray( IPIV ), convertIPIV( tc.ipiv ), 'ipiv' );
});
