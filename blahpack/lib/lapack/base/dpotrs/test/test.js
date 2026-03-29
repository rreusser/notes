/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpotrf = require( './../../dpotrf/lib/base.js' );
var dpotrs = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dpotrs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'dpotrs: lower_single_rhs', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'lower_single_rhs' );
	A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	B = new Float64Array( [ 1, 2, 3 ] );
	dpotrf( 'lower', 3, A, 1, 3, 0 );
	info = dpotrs( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dpotrs: upper_single_rhs', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'upper_single_rhs' );
	A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	B = new Float64Array( [ 1, 2, 3 ] );
	dpotrf( 'upper', 3, A, 1, 3, 0 );
	info = dpotrs( 'upper', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dpotrs: lower_multi_rhs', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'lower_multi_rhs' );
	A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	B = new Float64Array( [ 1, 0, 0, 0, 1, 0 ] );
	dpotrf( 'lower', 3, A, 1, 3, 0 );
	info = dpotrs( 'lower', 3, 2, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dpotrs: n_zero', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'n_zero' );
	A = new Float64Array( 1 );
	B = new Float64Array( 1 );
	info = dpotrs( 'lower', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpotrs: nrhs_zero', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'nrhs_zero' );
	A = new Float64Array( 9 );
	B = new Float64Array( 3 );
	info = dpotrs( 'lower', 3, 0, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dpotrs: one_by_one', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'one_by_one' );
	A = new Float64Array( [ 2 ] );
	B = new Float64Array( [ 6 ] );
	info = dpotrs( 'lower', 1, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dpotrs: upper_multi_rhs_3', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'upper_multi_rhs_3' );
	A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	B = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dpotrf( 'upper', 3, A, 1, 3, 0 );
	info = dpotrs( 'upper', 3, 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

// ndarray validation tests

test( 'dpotrs: ndarray throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ndarray( 'invalid', 3, 1, new Float64Array( 9 ), 1, 3, 0, new Float64Array( 3 ), 1, 3, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'dpotrs: ndarray throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ndarray( 'upper', -1, 1, new Float64Array( 9 ), 1, 3, 0, new Float64Array( 3 ), 1, 3, 0 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'dpotrs: ndarray throws RangeError for negative NRHS', function t() {
	assert.throws( function throws() {
		ndarray( 'upper', 3, -1, new Float64Array( 9 ), 1, 3, 0, new Float64Array( 3 ), 1, 3, 0 ); // eslint-disable-line max-len
	}, RangeError );
});
