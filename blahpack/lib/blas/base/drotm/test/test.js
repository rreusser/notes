/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var drotm = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'drotm.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'drotm: flag_neg1', function t() {
	var dparam;
	var tc;
	var dx;
	var dy;

	tc = findCase( 'flag_neg1' );
	dx = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	dy = new Float64Array( [ 4.0, 5.0, 6.0 ] );
	dparam = new Float64Array( [ -1.0, 2.0, -1.0, 3.0, 0.5 ] );

	drotm( 3, dx, 1, 0, dy, 1, 0, dparam, 1, 0 );
	assertArrayClose( toArray( dx ), tc.dx, 1e-14, 'dx' );
	assertArrayClose( toArray( dy ), tc.dy, 1e-14, 'dy' );
});

test( 'drotm: flag_zero', function t() {
	var dparam;
	var tc;
	var dx;
	var dy;

	tc = findCase( 'flag_zero' );
	dx = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	dy = new Float64Array( [ 4.0, 5.0, 6.0 ] );
	dparam = new Float64Array( [ 0.0, 0.0, -0.5, 0.25, 0.0 ] );

	drotm( 3, dx, 1, 0, dy, 1, 0, dparam, 1, 0 );
	assertArrayClose( toArray( dx ), tc.dx, 1e-14, 'dx' );
	assertArrayClose( toArray( dy ), tc.dy, 1e-14, 'dy' );
});

test( 'drotm: flag_one', function t() {
	var dparam;
	var tc;
	var dx;
	var dy;

	tc = findCase( 'flag_one' );
	dx = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	dy = new Float64Array( [ 4.0, 5.0, 6.0 ] );
	dparam = new Float64Array( [ 1.0, 0.5, 0.0, 0.0, 2.0 ] );

	drotm( 3, dx, 1, 0, dy, 1, 0, dparam, 1, 0 );
	assertArrayClose( toArray( dx ), tc.dx, 1e-14, 'dx' );
	assertArrayClose( toArray( dy ), tc.dy, 1e-14, 'dy' );
});

test( 'drotm: flag_neg2', function t() {
	var dparam;
	var tc;
	var dx;
	var dy;

	tc = findCase( 'flag_neg2' );
	dx = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	dy = new Float64Array( [ 4.0, 5.0, 6.0 ] );
	dparam = new Float64Array( [ -2.0, 0.0, 0.0, 0.0, 0.0 ] );

	drotm( 3, dx, 1, 0, dy, 1, 0, dparam, 1, 0 );
	assertArrayClose( toArray( dx ), tc.dx, 1e-14, 'dx' );
	assertArrayClose( toArray( dy ), tc.dy, 1e-14, 'dy' );
});

test( 'drotm: n_zero', function t() {
	var dparam;
	var tc;
	var dx;
	var dy;

	tc = findCase( 'n_zero' );
	dx = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	dy = new Float64Array( [ 4.0, 5.0, 6.0 ] );
	dparam = new Float64Array( [ -1.0, 2.0, -1.0, 3.0, 0.5 ] );

	drotm( 0, dx, 1, 0, dy, 1, 0, dparam, 1, 0 );
	assertArrayClose( toArray( dx ), tc.dx, 1e-14, 'dx' );
	assertArrayClose( toArray( dy ), tc.dy, 1e-14, 'dy' );
});

test( 'drotm: stride2', function t() {
	var dparam;
	var tc;
	var dx;
	var dy;

	tc = findCase( 'stride2' );
	dx = new Float64Array( [ 1.0, 0.0, 2.0, 0.0 ] );
	dy = new Float64Array( [ 3.0, 0.0, 4.0, 0.0 ] );
	dparam = new Float64Array( [ -1.0, 2.0, -1.0, 3.0, 0.5 ] );

	drotm( 2, dx, 2, 0, dy, 2, 0, dparam, 1, 0 );
	assertArrayClose( toArray( dx ), tc.dx, 1e-14, 'dx' );
	assertArrayClose( toArray( dy ), tc.dy, 1e-14, 'dy' );
});

test( 'drotm: neg_stride', function t() {
	var dparam;
	var tc;
	var dx;
	var dy;

	tc = findCase( 'neg_stride' );
	dx = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	dy = new Float64Array( [ 4.0, 5.0, 6.0 ] );
	dparam = new Float64Array( [ 0.0, 0.0, -0.5, 0.25, 0.0 ] );

	// Fortran call: drotm(3, dx, -1, dy, 1, dparam)

	// Negative stride for x means start from the end: offset = (N-1)*|stride| = 2
	drotm( 3, dx, -1, 2, dy, 1, 0, dparam, 1, 0 );
	assertArrayClose( toArray( dx ), tc.dx, 1e-14, 'dx' );
	assertArrayClose( toArray( dy ), tc.dy, 1e-14, 'dy' );
});
