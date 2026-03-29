/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zgeequ = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zgeequ.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'zgeequ: basic_3x3', function t() {
	var result;
	var tc;
	var A;
	var r;
	var c;

	tc = findCase( 'basic_3x3' );
	A = new Complex128Array([
		4,
		1,
		1,
		-1,
		0.5,
		0.2,
		1,
		0.5,
		3,
		2,
		1,
		-0.5,
		0.5,
		0.1,
		1,
		0.3,
		2,
		1
	]);
	r = new Float64Array( 3 );
	c = new Float64Array( 3 );
	result = zgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: m_zero', function t() {
	var result;
	var tc;
	var A;
	var r;
	var c;

	tc = findCase( 'm_zero' );
	A = new Complex128Array( 1 );
	r = new Float64Array( 1 );
	c = new Float64Array( 1 );
	result = zgeequ( 0, 3, A, 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: n_zero', function t() {
	var result;
	var tc;
	var A;
	var r;
	var c;

	tc = findCase( 'n_zero' );
	A = new Complex128Array( 1 );
	r = new Float64Array( 1 );
	c = new Float64Array( 1 );
	result = zgeequ( 3, 0, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: zero_row', function t() {
	var result;
	var tc;
	var A;
	var r;
	var c;

	tc = findCase( 'zero_row' );
	A = new Complex128Array([
		4,
		1,
		0,
		0,
		1,
		0,
		1,
		0.5,
		0,
		0,
		2,
		0,
		0.5,
		0.1,
		0,
		0,
		3,
		0
	]);
	r = new Float64Array( 3 );
	c = new Float64Array( 3 );
	result = zgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: 1x1', function t() {
	var result;
	var tc;
	var A;
	var r;
	var c;

	tc = findCase( '1x1' );
	A = new Complex128Array( [ 5, 3 ] );
	r = new Float64Array( 1 );
	c = new Float64Array( 1 );
	result = zgeequ( 1, 1, A, 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: poorly_scaled', function t() {
	var result;
	var tc;
	var A;
	var r;
	var c;

	tc = findCase( 'poorly_scaled' );
	A = new Complex128Array([
		1e6,
		0,
		1,
		0,
		1,
		0,
		1,
		0,
		1e-3,
		0,
		1,
		0,
		1,
		0,
		1,
		0,
		1e3,
		0
	]);
	r = new Float64Array( 3 );
	c = new Float64Array( 3 );
	result = zgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: rect_2x3', function t() {
	var result;
	var tc;
	var A;
	var r;
	var c;

	tc = findCase( 'rect_2x3' );
	A = new Complex128Array([
		2,
		1,
		1,
		-1,
		3,
		0,
		0.5,
		0.5,
		1,
		1,
		4,
		2
	]);
	r = new Float64Array( 2 );
	c = new Float64Array( 3 );
	result = zgeequ( 2, 3, A, 1, 2, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});
