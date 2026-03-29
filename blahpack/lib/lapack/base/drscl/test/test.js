/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var drscl = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'drscl.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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


// TESTS //

test( 'drscl: basic', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 2.0, 4.0, 6.0, 8.0 ] );
	drscl( 4, 2.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: half', function t() {
	var tc = findCase( 'half' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	drscl( 3, 0.5, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Float64Array( [ 99.0 ] );
	drscl( 0, 2.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: n_one', function t() {
	var tc = findCase( 'n_one' );
	var x = new Float64Array( [ 10.0 ] );
	drscl( 1, 5.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: stride2', function t() {
	var tc = findCase( 'stride2' );
	var x = new Float64Array( [ 10.0, 99.0, 20.0, 99.0, 30.0 ] );
	drscl( 3, 10.0, x, 2, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: identity', function t() {
	var tc = findCase( 'identity' );
	var x = new Float64Array( [ 3.0, 7.0 ] );
	drscl( 2, 1.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: large_sa', function t() {
	var tc = findCase( 'large_sa' );
	var x = new Float64Array( [ 1.0, 2.0 ] );
	drscl( 2, 1e300, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: small_sa', function t() {
	var tc = findCase( 'small_sa' );
	var x = new Float64Array( [ 1e-300, 2e-300 ] );
	drscl( 2, 1e-300, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: very_large_sa (triggers SMLNUM branch)', function t() {
	var tc = findCase( 'very_large_sa' );
	var x = new Float64Array( [ 1.0, 2.0 ] );
	drscl( 2, 1e308, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: very_small_sa (triggers BIGNUM branch)', function t() {
	var tc = findCase( 'very_small_sa' );
	var x = new Float64Array( [ 1e-308, 2e-308 ] );
	drscl( 2, 1e-309, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: negative_sa', function t() {
	var tc = findCase( 'negative_sa' );
	var x = new Float64Array( [ 6.0, -9.0, 12.0 ] );
	drscl( 3, -3.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: offset', function t() {
	// Test with non-zero offset
	var x = new Float64Array( [ 999.0, 10.0, 20.0, 30.0 ] );
	drscl( 3, 10.0, x, 1, 1 );
	assert.equal( x[ 0 ], 999.0 ); // untouched
	assertClose( x[ 1 ], 1.0, 1e-14, 'x[1]' );
	assertClose( x[ 2 ], 2.0, 1e-14, 'x[2]' );
	assertClose( x[ 3 ], 3.0, 1e-14, 'x[3]' );
});
