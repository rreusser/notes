/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsytri = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dsytri.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	return fixture.find( function find( t ) {
		return t.name === name;
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
* Converts Fortran 1-based IPIV to JS 0-based IPIV.
*
* @private
* @param {Array} ipiv - Fortran 1-based pivot indices
* @returns {Int32Array} 0-based pivot indices
*/
function convertIPIV( ipiv ) {
	var out;
	var i;
	out = new Int32Array( ipiv.length );
	for ( i = 0; i < ipiv.length; i++ ) {
		if ( ipiv[ i ] >= 0 ) {
			out[ i ] = ipiv[ i ] - 1;
		} else {
			out[ i ] = ~( ( -ipiv[ i ] ) - 1 );
		}
	}
	return out;
}

/**
* Creates an NxN Float64Array from column-major flat fixture data.
*
* @private
* @param {Array} arr - flat array from fixture
* @param {NonNegativeInteger} N - matrix order
* @returns {Float64Array} matrix data
*/
function matFromFixture( arr, N ) {
	return new Float64Array( arr.slice( 0, N * N ) );
}

/**
* Extracts column-major flat values from a full-storage matrix.
*
* @private
* @param {Float64Array} A - matrix data
* @param {NonNegativeInteger} N - matrix order
* @returns {Float64Array} flat array copy
*/
function matToFlat( A, N ) {
	var out;
	var i;
	out = new Float64Array( N * N );
	for ( i = 0; i < N * N; i++ ) {
		out[ i ] = A[ i ];
	}
	return out;
}


// TESTS //

test( 'dsytri: 3x3_upper', function t() {
	var expected;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var N;

	tc = findCase( '3x3_upper' );
	N = 3;
	A = matFromFixture( tc.a_fact, N );
	ipiv = convertIPIV( tc.ipiv.slice( 0, N ) );
	expected = matFromFixture( tc.a, N );
	work = new Float64Array( N );
	info = dsytri( 'upper', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( matToFlat( A, N ), expected, 1e-14, 'a' );
});

test( 'dsytri: 3x3_lower', function t() {
	var expected;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var N;

	tc = findCase( '3x3_lower' );
	N = 3;
	A = matFromFixture( tc.a_fact, N );
	ipiv = convertIPIV( tc.ipiv.slice( 0, N ) );
	expected = matFromFixture( tc.a, N );
	work = new Float64Array( N );
	info = dsytri( 'lower', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( matToFlat( A, N ), expected, 1e-14, 'a' );
});

test( 'dsytri: 4x4_upper', function t() {
	var expected;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var N;

	tc = findCase( '4x4_upper' );
	N = 4;
	A = matFromFixture( tc.a_fact, N );
	ipiv = convertIPIV( tc.ipiv.slice( 0, N ) );
	expected = matFromFixture( tc.a, N );
	work = new Float64Array( N );
	info = dsytri( 'upper', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( matToFlat( A, N ), expected, 1e-14, 'a' );
});

test( 'dsytri: 4x4_lower', function t() {
	var expected;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var N;

	tc = findCase( '4x4_lower' );
	N = 4;
	A = matFromFixture( tc.a_fact, N );
	ipiv = convertIPIV( tc.ipiv.slice( 0, N ) );
	expected = matFromFixture( tc.a, N );
	work = new Float64Array( N );
	info = dsytri( 'lower', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( matToFlat( A, N ), expected, 1e-14, 'a' );
});

test( 'dsytri: n_zero', function t() {
	var work;
	var ipiv;
	var info;
	var A;

	A = new Float64Array( 1 );
	ipiv = new Int32Array( 1 );
	work = new Float64Array( 1 );
	info = dsytri( 'upper', 0, A, 1, 1, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytri: n_one', function t() {
	var expected;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var N;

	tc = findCase( 'n_one' );
	N = 1;
	A = matFromFixture( tc.a_fact, N );
	ipiv = convertIPIV( tc.ipiv.slice( 0, N ) );
	expected = matFromFixture( tc.a, N );
	work = new Float64Array( N );
	info = dsytri( 'upper', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( matToFlat( A, N ), expected, 1e-14, 'a' );
});

test( 'dsytri: 4x4_indef_2x2_upper', function t() {
	var expected;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var N;

	tc = findCase( '4x4_indef_2x2_upper' );
	N = 4;
	A = matFromFixture( tc.a_fact, N );
	ipiv = convertIPIV( tc.ipiv.slice( 0, N ) );
	expected = matFromFixture( tc.a, N );
	work = new Float64Array( N );
	info = dsytri( 'upper', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( matToFlat( A, N ), expected, 1e-14, 'a' );
});

test( 'dsytri: 4x4_indef_2x2_lower', function t() {
	var expected;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var N;

	tc = findCase( '4x4_indef_2x2_lower' );
	N = 4;
	A = matFromFixture( tc.a_fact, N );
	ipiv = convertIPIV( tc.ipiv.slice( 0, N ) );
	expected = matFromFixture( tc.a, N );
	work = new Float64Array( N );
	info = dsytri( 'lower', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( matToFlat( A, N ), expected, 1e-14, 'a' );
});

test( 'dsytri: 3x3_indef_upper', function t() {
	var expected;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var N;

	tc = findCase( '3x3_indef_upper' );
	N = 3;
	A = matFromFixture( tc.a_fact, N );
	ipiv = convertIPIV( tc.ipiv.slice( 0, N ) );
	expected = matFromFixture( tc.a, N );
	work = new Float64Array( N );
	info = dsytri( 'upper', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( matToFlat( A, N ), expected, 1e-14, 'a' );
});

test( 'dsytri: 3x3_indef_lower', function t() {
	var expected;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var N;

	tc = findCase( '3x3_indef_lower' );
	N = 3;
	A = matFromFixture( tc.a_fact, N );
	ipiv = convertIPIV( tc.ipiv.slice( 0, N ) );
	expected = matFromFixture( tc.a, N );
	work = new Float64Array( N );
	info = dsytri( 'lower', N, A, 1, N, 0, ipiv, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( matToFlat( A, N ), expected, 1e-14, 'a' );
});
