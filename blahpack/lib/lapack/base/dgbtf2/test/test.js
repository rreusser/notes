/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgbtf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dgbtf2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'dgbtf2: N=0 quick return', function t() {
	var IPIV;
	var info;
	var AB;

	AB = new Float64Array( 16 );
	IPIV = new Int32Array( 4 );
	info = dgbtf2( 3, 0, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dgbtf2: M=0 quick return', function t() {
	var IPIV;
	var info;
	var AB;

	AB = new Float64Array( 16 );
	IPIV = new Int32Array( 4 );
	info = dgbtf2( 0, 3, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dgbtf2: tridiag_4x4', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var i;

	tc = findCase( 'tridiag_4x4' );
	AB = new Float64Array([
		0.0,
		0.0,
		4.0,
		-1.0,
		0.0,
		-1.0,
		4.0,
		-1.0,
		0.0,
		-1.0,
		4.0,
		-1.0,
		0.0,
		-1.0,
		4.0,
		0.0
	]);
	IPIV = new Int32Array( 4 );
	info = dgbtf2( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( AB ), tc.AB, 1e-14, 'AB' );
	for ( i = 0; i < 4; i++ ) {
		assert.equal( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'dgbtf2: pentadiag_5x5', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var i;

	tc = findCase( 'pentadiag_5x5' );
	AB = new Float64Array( 7 * 5 );
	AB[ 4 ] = 6.0;
	AB[ 5 ] = -2.0;
	AB[ 6 ] = 1.0;
	AB[ 7 + 3 ] = -2.0;
	AB[ 7 + 4 ] = 6.0;
	AB[ 7 + 5 ] = -2.0;
	AB[ 7 + 6 ] = 1.0;
	AB[ 14 + 2 ] = 1.0;
	AB[ 14 + 3 ] = -2.0;
	AB[ 14 + 4 ] = 6.0;
	AB[ 14 + 5 ] = -2.0;
	AB[ 14 + 6 ] = 1.0;
	AB[ 21 + 2 ] = 1.0;
	AB[ 21 + 3 ] = -2.0;
	AB[ 21 + 4 ] = 6.0;
	AB[ 21 + 5 ] = -2.0;
	AB[ 28 + 2 ] = 1.0;
	AB[ 28 + 3 ] = -2.0;
	AB[ 28 + 4 ] = 6.0;
	IPIV = new Int32Array( 5 );
	info = dgbtf2( 5, 5, 2, 2, AB, 1, 7, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( AB ), tc.AB, 1e-14, 'AB' );
	for ( i = 0; i < 5; i++ ) {
		assert.equal( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'dgbtf2: kl1_ku2_3x3', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var i;

	tc = findCase( 'kl1_ku2_3x3' );
	AB = new Float64Array( 5 * 3 );
	AB[ 3 ] = 5.0;
	AB[ 4 ] = 2.0;
	AB[ 5 + 2 ] = 3.0;
	AB[ 5 + 3 ] = 6.0;
	AB[ 5 + 4 ] = 1.0;
	AB[ 10 + 1 ] = 1.0;
	AB[ 10 + 2 ] = 4.0;
	AB[ 10 + 3 ] = 7.0;
	IPIV = new Int32Array( 3 );
	info = dgbtf2( 3, 3, 1, 2, AB, 1, 5, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( AB ), tc.AB, 1e-14, 'AB' );
	for ( i = 0; i < 3; i++ ) {
		assert.equal( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'dgbtf2: one_by_one', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;

	tc = findCase( 'one_by_one' );
	AB = new Float64Array( [ 7.0 ] );
	IPIV = new Int32Array( 1 );
	info = dgbtf2( 1, 1, 0, 0, AB, 1, 1, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( AB ), tc.AB, 1e-14, 'AB' );
	assert.equal( IPIV[ 0 ], tc.ipiv[ 0 ] - 1, 'ipiv[0]' );
});

test( 'dgbtf2: singular', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;

	tc = findCase( 'singular' );
	AB = new Float64Array([
		0.0,
		0.0,
		1.0,
		0.0
	]);
	IPIV = new Int32Array( 2 );
	info = dgbtf2( 2, 2, 0, 1, AB, 1, 2, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dgbtf2: tall_5x3', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var i;

	tc = findCase( 'tall_5x3' );
	AB = new Float64Array( 4 * 3 );
	AB[ 2 ] = 4.0;
	AB[ 3 ] = -1.0;
	AB[ 4 + 1 ] = -1.0;
	AB[ 4 + 2 ] = 4.0;
	AB[ 4 + 3 ] = -1.0;
	AB[ 8 + 1 ] = -1.0;
	AB[ 8 + 2 ] = 4.0;
	AB[ 8 + 3 ] = -1.0;
	IPIV = new Int32Array( 3 );
	info = dgbtf2( 5, 3, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( AB ), tc.AB, 1e-14, 'AB' );
	for ( i = 0; i < 3; i++ ) {
		assert.equal( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'dgbtf2: pivot_2x2', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var i;

	tc = findCase( 'pivot_2x2' );
	AB = new Float64Array( 4 * 2 );
	AB[ 2 ] = 1.0;
	AB[ 3 ] = 3.0;
	AB[ 4 + 1 ] = 2.0;
	AB[ 4 + 2 ] = 4.0;
	IPIV = new Int32Array( 2 );
	info = dgbtf2( 2, 2, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( AB ), tc.AB, 1e-14, 'AB' );
	for ( i = 0; i < 2; i++ ) {
		assert.equal( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});
