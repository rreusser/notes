/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlauum = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlauum.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Extracts a column-major flat array from a Float64Array matrix.
*
* @private
* @param {Float64Array} A - matrix
* @param {integer} N - order
* @param {integer} sa1 - stride of first dimension
* @param {integer} sa2 - stride of second dimension
* @param {integer} offset - starting offset
* @returns {Array} column-major flat array
*/
function extractColMajor( A, N, sa1, sa2, offset ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			out.push( A[ offset + i * sa1 + j * sa2 ] );
		}
	}
	return out;
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

test( 'dlauum: upper 4x4 (U*U^T)', function t() {
	var info;
	var tc;
	var N;
	var A;

	tc = findCase( 'upper_4' );
	N = 4;
	A = new Float64Array( N * N );
	A[ 0 ] = 2.0;
	A[ 4 ] = 1.0;
	A[ 8 ] = 3.0;
	A[ 12 ] = 2.0;
	A[ 5 ] = 4.0;
	A[ 9 ] = 1.0;
	A[ 13 ] = 3.0;
	A[ 10 ] = 5.0;
	A[ 14 ] = 1.0;
	A[ 15 ] = 6.0;
	info = dlauum( 'upper', N, A, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( extractColMajor( A, N, 1, N, 0 ), tc.a, 1e-14, 'a' );
});

test( 'dlauum: lower 4x4 (L^T*L)', function t() {
	var info;
	var tc;
	var N;
	var A;

	tc = findCase( 'lower_4' );
	N = 4;
	A = new Float64Array( N * N );
	A[ 0 ] = 2.0;
	A[ 1 ] = 1.0;
	A[ 2 ] = 3.0;
	A[ 3 ] = 2.0;
	A[ 5 ] = 4.0;
	A[ 6 ] = 1.0;
	A[ 7 ] = 3.0;
	A[ 10 ] = 5.0;
	A[ 11 ] = 1.0;
	A[ 15 ] = 6.0;
	info = dlauum( 'lower', N, A, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( extractColMajor( A, N, 1, N, 0 ), tc.a, 1e-14, 'a' );
});

test( 'dlauum: N=1 upper', function t() {
	var info;
	var tc;
	var A;

	tc = findCase( 'n1_upper' );
	A = new Float64Array( [ 3.0 ] );
	info = dlauum( 'upper', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( A ), tc.a, 1e-14, 'a' );
});

test( 'dlauum: N=1 lower', function t() {
	var info;
	var tc;
	var A;

	tc = findCase( 'n1_lower' );
	A = new Float64Array( [ 5.0 ] );
	info = dlauum( 'lower', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( A ), tc.a, 1e-14, 'a' );
});

test( 'dlauum: N=0 quick return', function t() {
	var info;
	var tc;
	var A;

	tc = findCase( 'n0' );
	A = new Float64Array( 1 );
	info = dlauum( 'upper', 0, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dlauum: upper 35x35 (blocked path)', function t() {
	var info;
	var tc;
	var N;
	var A;
	var i;
	var j;

	tc = findCase( 'upper_35' );
	N = 35;
	A = new Float64Array( N * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i <= j; i++ ) {
			if ( i === j ) {
				A[ i + j * N ] = N + 1;
			} else {
				A[ i + j * N ] = 1.0 / ( j - i + 1 );
			}
		}
	}
	info = dlauum( 'upper', N, A, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( extractColMajor( A, N, 1, N, 0 ), tc.a, 1e-12, 'a' );
});

test( 'dlauum: lower 35x35 (blocked path)', function t() {
	var info;
	var tc;
	var N;
	var A;
	var i;
	var j;

	tc = findCase( 'lower_35' );
	N = 35;
	A = new Float64Array( N * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = j; i < N; i++ ) {
			if ( i === j ) {
				A[ i + j * N ] = N + 1;
			} else {
				A[ i + j * N ] = 1.0 / ( i - j + 1 );
			}
		}
	}
	info = dlauum( 'lower', N, A, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( extractColMajor( A, N, 1, N, 0 ), tc.a, 1e-12, 'a' );
});
