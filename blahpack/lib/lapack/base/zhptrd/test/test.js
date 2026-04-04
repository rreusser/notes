/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhptrd = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zhptrd.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {Object} test case data
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
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr;
	var denom;

	denom = Math.max( Math.abs( expected ), 1.0 );
	relErr = Math.abs( actual - expected ) / denom;
	assert.ok( relErr <= tol, msg + ': got ' + actual + ' expected ' + expected );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {Array} actual - actual value
* @param {Array} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;

	assert.equal( actual.length, expected.length, msg + ' length' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Converts a typed array slice to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @param {NonNegativeInteger} start - start index
* @param {NonNegativeInteger} end - end index
* @returns {Array} output array
*/
function toArray( arr, start, end ) {
	var out = [];
	var i;

	for ( i = start; i < end; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'zhptrd: upper, 4x4', function t() {
	var tauv;
	var info;
	var TAU;
	var apv;
	var tc;
	var AP;
	var d;
	var e;

	tc = findCase( 'zhptrd_4x4_upper' );
	AP = new Complex128Array([
		4,
		0,
		1,
		-1,
		2,
		0,
		-2,
		1,
		0,
		0,
		3,
		0,
		2,
		0,
		1,
		-1,
		-2,
		-1,
		-1,
		0
	]);
	d = new Float64Array( 4 );
	e = new Float64Array( 3 );
	TAU = new Complex128Array( 3 );
	info = zhptrd( 'upper', 4, AP, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, 0, 4 ), tc.D, 1e-13, 'D' );
	assertArrayClose( toArray( e, 0, 3 ), tc.E, 1e-13, 'E' );
	tauv = reinterpret( TAU, 0 );
	assertArrayClose( toArray( tauv, 0, 6 ), tc.TAU, 1e-13, 'TAU' );
	apv = reinterpret( AP, 0 );
	assertArrayClose( toArray( apv, 0, 20 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhptrd: lower, 4x4', function t() {
	var tauv;
	var info;
	var TAU;
	var apv;
	var tc;
	var AP;
	var d;
	var e;

	tc = findCase( 'zhptrd_4x4_lower' );
	AP = new Complex128Array([
		4,
		0,
		1,
		1,
		-2,
		-1,
		2,
		0,
		2,
		0,
		0,
		0,
		1,
		-1,
		3,
		0,
		-2,
		1,
		-1,
		0
	]);
	d = new Float64Array( 4 );
	e = new Float64Array( 3 );
	TAU = new Complex128Array( 3 );
	info = zhptrd( 'lower', 4, AP, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, 0, 4 ), tc.D, 1e-13, 'D' );
	assertArrayClose( toArray( e, 0, 3 ), tc.E, 1e-13, 'E' );
	tauv = reinterpret( TAU, 0 );
	assertArrayClose( toArray( tauv, 0, 6 ), tc.TAU, 1e-13, 'TAU' );
	apv = reinterpret( AP, 0 );
	assertArrayClose( toArray( apv, 0, 20 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhptrd: N=1', function t() {
	var info;
	var TAU;
	var tc;
	var AP;
	var d;
	var e;

	tc = findCase( 'zhptrd_1x1' );
	AP = new Complex128Array( [ 5, 0 ] );
	d = new Float64Array( 1 );
	e = new Float64Array( 0 );
	TAU = new Complex128Array( 0 );
	info = zhptrd( 'upper', 1, AP, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( d, 0, 1 ), tc.D, 1e-13, 'D' );
});

test( 'zhptrd: N=0', function t() {
	var info;
	var TAU;
	var tc;
	var AP;
	var d;
	var e;

	tc = findCase( 'zhptrd_0x0' );
	AP = new Complex128Array( 0 );
	d = new Float64Array( 0 );
	e = new Float64Array( 0 );
	TAU = new Complex128Array( 0 );
	info = zhptrd( 'upper', 0, AP, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, tc.info );
});
