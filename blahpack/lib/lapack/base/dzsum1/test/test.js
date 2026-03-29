/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var dzsum1 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dzsum1.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
}


// TESTS //

test( 'dzsum1: main export is a function', function t() {
	assert.strictEqual( typeof dzsum1, 'function' );
});

test( 'dzsum1: basic 3-element vector', function t() {
	var result;
	var tc;
	var zx;

	tc = findCase( 'basic_3' );
	zx = new Complex128Array( [ 3, 4, 1, 0, 0, 1 ] );
	result = dzsum1( 3, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: stride=2', function t() {
	var result;
	var tc;
	var zx;

	tc = findCase( 'stride2' );
	zx = new Complex128Array( [ 3, 4, 99, 99, 5, 12, 99, 99, 8, 15 ] );
	result = dzsum1( 3, zx, 2, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: N=0 returns 0', function t() {
	var result;
	var tc;
	var zx;

	tc = findCase( 'n_zero' );
	zx = new Complex128Array( [ 1, 2 ] );
	result = dzsum1( 0, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: N=1', function t() {
	var result;
	var tc;
	var zx;

	tc = findCase( 'n_one' );
	zx = new Complex128Array( [ 6, 8 ] );
	result = dzsum1( 1, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: all zeros', function t() {
	var result;
	var tc;
	var zx;

	tc = findCase( 'all_zeros' );
	zx = new Complex128Array( [ 0, 0, 0, 0, 0, 0 ] );
	result = dzsum1( 3, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: purely real', function t() {
	var result;
	var tc;
	var zx;

	tc = findCase( 'purely_real' );
	zx = new Complex128Array( [ 3, 0, -4, 0, 5, 0 ] );
	result = dzsum1( 3, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: purely imaginary', function t() {
	var result;
	var tc;
	var zx;

	tc = findCase( 'purely_imag' );
	zx = new Complex128Array( [ 0, 2, 0, -3 ] );
	result = dzsum1( 2, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzsum1: nonzero offset', function t() {
	var result;
	var zx;

	zx = new Complex128Array( [ 99, 99, 3, 4, 1, 0, 0, 1 ] );
	result = dzsum1( 3, zx, 1, 1 );
	assertClose( result, 7.0, 1e-14, 'result' );
});
