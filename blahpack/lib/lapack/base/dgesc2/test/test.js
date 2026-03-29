/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgetc2 = require( '../../dgetc2/lib/base.js' );
var dgesc2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dgesc2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'dgesc2: basic_2x2', function t() {
	var scale;
	var IPIV;
	var JPIV;
	var LDA;
	var RHS;
	var tc;
	var N;
	var A;

	tc = findCase( 'basic_2x2' );
	N = 2;
	LDA = 4;
	A = new Float64Array( LDA * N );
	IPIV = new Int32Array( N );
	JPIV = new Int32Array( N );
	RHS = new Float64Array( N );
	scale = new Float64Array( 1 );
	A[ 0*LDA + 0 ] = 4.0;
	A[ 1*LDA + 0 ] = 3.0;
	A[ 0*LDA + 1 ] = 2.0;
	A[ 1*LDA + 1 ] = 1.0;
	dgetc2( N, A, 1, LDA, 0, IPIV, 1, 0, JPIV, 1, 0 );
	RHS[ 0 ] = 10.0;
	RHS[ 1 ] = 4.0;
	dgesc2( N, A, 1, LDA, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( RHS ), tc.rhs, 1e-14, 'rhs' );
});

test( 'dgesc2: basic_3x3', function t() {
	var scale;
	var IPIV;
	var JPIV;
	var LDA;
	var RHS;
	var tc;
	var N;
	var A;

	tc = findCase( 'basic_3x3' );
	N = 3;
	LDA = 4;
	A = new Float64Array( LDA * N );
	IPIV = new Int32Array( N );
	JPIV = new Int32Array( N );
	RHS = new Float64Array( N );
	scale = new Float64Array( 1 );
	A[ 0*LDA + 0 ] = 2.0;
	A[ 1*LDA + 0 ] = 1.0;
	A[ 2*LDA + 0 ] = 1.0;
	A[ 0*LDA + 1 ] = 4.0;
	A[ 1*LDA + 1 ] = 3.0;
	A[ 2*LDA + 1 ] = 3.0;
	A[ 0*LDA + 2 ] = 8.0;
	A[ 1*LDA + 2 ] = 7.0;
	A[ 2*LDA + 2 ] = 9.0;
	dgetc2( N, A, 1, LDA, 0, IPIV, 1, 0, JPIV, 1, 0 );
	RHS[ 0 ] = 4.0;
	RHS[ 1 ] = 10.0;
	RHS[ 2 ] = 24.0;
	dgesc2( N, A, 1, LDA, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( RHS ), tc.rhs, 1e-14, 'rhs' );
});

test( 'dgesc2: basic_4x4', function t() {
	var scale;
	var IPIV;
	var JPIV;
	var LDA;
	var RHS;
	var tc;
	var N;
	var A;

	tc = findCase( 'basic_4x4' );
	N = 4;
	LDA = 4;
	A = new Float64Array( LDA * N );
	IPIV = new Int32Array( N );
	JPIV = new Int32Array( N );
	RHS = new Float64Array( N );
	scale = new Float64Array( 1 );
	A[ 0*LDA + 0 ] = 5.0;
	A[ 1*LDA + 0 ] = 7.0;
	A[ 2*LDA + 0 ] = 6.0;
	A[ 3*LDA + 0 ] = 5.0;
	A[ 0*LDA + 1 ] = 7.0;
	A[ 1*LDA + 1 ] = 10.0;
	A[ 2*LDA + 1 ] = 8.0;
	A[ 3*LDA + 1 ] = 7.0;
	A[ 0*LDA + 2 ] = 6.0;
	A[ 1*LDA + 2 ] = 8.0;
	A[ 2*LDA + 2 ] = 10.0;
	A[ 3*LDA + 2 ] = 9.0;
	A[ 0*LDA + 3 ] = 5.0;
	A[ 1*LDA + 3 ] = 7.0;
	A[ 2*LDA + 3 ] = 9.0;
	A[ 3*LDA + 3 ] = 10.0;
	dgetc2( N, A, 1, LDA, 0, IPIV, 1, 0, JPIV, 1, 0 );
	RHS[ 0 ] = 23.0;
	RHS[ 1 ] = 32.0;
	RHS[ 2 ] = 33.0;
	RHS[ 3 ] = 31.0;
	dgesc2( N, A, 1, LDA, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( RHS ), tc.rhs, 1e-14, 'rhs' );
});

test( 'dgesc2: n_equals_1', function t() {
	var scale;
	var IPIV;
	var JPIV;
	var LDA;
	var RHS;
	var tc;
	var N;
	var A;

	tc = findCase( 'n_equals_1' );
	N = 1;
	LDA = 4;
	A = new Float64Array( LDA * 1 );
	IPIV = new Int32Array( 1 );
	JPIV = new Int32Array( 1 );
	RHS = new Float64Array( 1 );
	scale = new Float64Array( 1 );
	A[ 0 ] = 3.0;
	dgetc2( N, A, 1, LDA, 0, IPIV, 1, 0, JPIV, 1, 0 );
	RHS[ 0 ] = 9.0;
	dgesc2( N, A, 1, LDA, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( RHS ), tc.rhs, 1e-14, 'rhs' );
});
