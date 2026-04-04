/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpftrf = require( '../../dpftrf/lib/base.js' );
var dpftri = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dpftri.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync, max-len
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
* @param {number} actual - actual value
* @param {number} expected - expected value
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
* @param {Array} actual - actual value
* @param {Array} expected - expected value
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
* Converts a Float64Array to a plain array.
*
* @private
* @param {Float64Array} arr - typed array
* @param {number} len - number of elements to copy
* @returns {Array} plain array
*/
function toArray( arr, len ) {
	var out = [];
	var i;
	for ( i = 0; i < len; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

/**
* Runs a standard test case: factorize with dpftrf, then invert with dpftri.
*
* @private
* @param {string} name - test case name
* @param {string} transr - `'no-transpose'` or `'transpose'`
* @param {string} uplo - `'upper'` or `'lower'`
* @param {NonNegativeInteger} N - matrix order
*/
function runTest( name, transr, uplo, N ) {
	var expected;
	var actual;
	var info;
	var tc;
	var a;
	var i;

	tc = findCase( name );
	a = new Float64Array( tc.input.length );
	for ( i = 0; i < tc.input.length; i++ ) {
		a[ i ] = tc.input[ i ];
	}

	// Factorize...
	info = dpftrf( transr, uplo, N, a, 1, 0 );
	assert.equal( info, 0, 'dpftrf info' );

	// Invert...
	info = dpftri( transr, uplo, N, a, 1, 0 );
	assert.equal( info, tc.info, 'info' );

	expected = tc.a;
	actual = toArray( a, expected.length );
	assertArrayClose( actual, expected, 1e-14, 'a' );
}


// TESTS //

test( 'dpftri is a function', function t() {
	assert.equal( typeof dpftri, 'function' );
} );

test( 'dpftri: lower_odd_normal', function t() {
	runTest( 'lower_odd_normal', 'no-transpose', 'lower', 3 );
} );

test( 'dpftri: upper_odd_normal', function t() {
	runTest( 'upper_odd_normal', 'no-transpose', 'upper', 3 );
} );

test( 'dpftri: lower_odd_trans', function t() {
	runTest( 'lower_odd_trans', 'transpose', 'lower', 3 );
} );

test( 'dpftri: upper_odd_trans', function t() {
	runTest( 'upper_odd_trans', 'transpose', 'upper', 3 );
} );

test( 'dpftri: lower_even_normal', function t() {
	runTest( 'lower_even_normal', 'no-transpose', 'lower', 4 );
} );

test( 'dpftri: upper_even_normal', function t() {
	runTest( 'upper_even_normal', 'no-transpose', 'upper', 4 );
} );

test( 'dpftri: lower_even_trans', function t() {
	runTest( 'lower_even_trans', 'transpose', 'lower', 4 );
} );

test( 'dpftri: upper_even_trans', function t() {
	runTest( 'upper_even_trans', 'transpose', 'upper', 4 );
} );

test( 'dpftri: n_zero', function t() {
	var info;
	var a;

	a = new Float64Array( 1 );
	info = dpftri( 'no-transpose', 'lower', 0, a, 1, 0 );
	assert.equal( info, 0, 'info for N=0' );
} );

test( 'dpftri: n_one', function t() {
	var info;
	var tc;
	var a;

	tc = findCase( 'n_one' );
	a = new Float64Array( [ 9.0 ] );
	info = dpftrf( 'no-transpose', 'lower', 1, a, 1, 0 );
	assert.equal( info, 0, 'dpftrf info for N=1' );
	info = dpftri( 'no-transpose', 'lower', 1, a, 1, 0 );
	assert.equal( info, tc.info, 'info for N=1' );
	assertArrayClose( toArray( a, 1 ), tc.a, 1e-14, 'a for N=1' );
} );

test( 'dpftri: lower_5_normal', function t() {
	runTest( 'lower_5_normal', 'no-transpose', 'lower', 5 );
} );

test( 'dpftri: upper_5_trans', function t() {
	runTest( 'upper_5_trans', 'transpose', 'upper', 5 );
} );

test( 'dpftri: singular (info > 0)', function t() {
	var info;
	var tc;
	var a;

	tc = findCase( 'singular' );
	a = new Float64Array( [ 1.0, 0.5, 0.3, 0.0, 1.0, 0.2 ] );
	info = dpftri( 'no-transpose', 'lower', 3, a, 1, 0 );
	assert.equal( info, tc.info, 'info for singular case' );
} );
