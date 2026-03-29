/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgeqr2 = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dgeqr2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );

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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
		if ( relErr > tol ) {
			throw new Error( msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
		}
	}
}

/**
* ExtractMatrix.
*
* @private
* @param {*} A - A
* @param {*} M - M
* @param {*} N - N
* @param {*} lda - lda
* @returns {*} result
*/
function extractMatrix( A, M, N, lda ) {
	var out = [];
	var i; var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( A[ i + j * lda ] );
		}
	}
	return out;
}

test( 'dgeqr2: 3x2', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '3x2' );
	A = new Float64Array( 3 * 2 );
	A[ 0 ] = 1;
	A[ 1 ] = 3;
	A[ 2 ] = 5;
	A[ 3 ] = 2;
	A[ 4 ] = 4;
	A[ 5 ] = 6;
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 2 );
	info = dgeqr2( 3, 2, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( extractMatrix( A, 3, 2, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-14, 'TAU' );
});

test( 'dgeqr2: 2x2', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '2x2' );
	A = new Float64Array( 2 * 2 );
	A[ 0 ] = 4;
	A[ 1 ] = 3;
	A[ 2 ] = 1;
	A[ 3 ] = 2;
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 2 );
	info = dgeqr2( 2, 2, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( extractMatrix( A, 2, 2, 2 ), tc.A, 1e-14, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-14, 'TAU' );
});

test( 'dgeqr2: N=0', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'n_zero' );
	A = new Float64Array( 2 );
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 2 );
	info = dgeqr2( 2, 0, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
});

test( 'dgeqr2: M=0', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'm_zero' );
	A = new Float64Array( 2 );
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 2 );
	info = dgeqr2( 0, 2, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
});

test( 'dgeqr2: 4x3', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '4x3' );
	A = new Float64Array( 4 * 3 );
	A[ 0 ] = 2;
	A[ 1 ] = 1;
	A[ 2 ] = 3;
	A[ 3 ] = 1;
	A[ 4 ] = 1;
	A[ 5 ] = 4;
	A[ 6 ] = 2;
	A[ 7 ] = 3;
	A[ 8 ] = 3;
	A[ 9 ] = 2;
	A[ 10 ] = 5;
	A[ 11 ] = 1;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );
	info = dgeqr2( 4, 3, A, 1, 4, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( extractMatrix( A, 4, 3, 4 ), tc.A, 1e-14, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-14, 'TAU' );
});
