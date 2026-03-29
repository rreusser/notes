/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var dlaev2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlaev2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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


// TESTS //

test( 'dlaev2: identity matrix', function t() {
	var out;
	var tc;

	tc = findCase( 'identity' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: diagonal matrix', function t() {
	var out;
	var tc;

	tc = findCase( 'diagonal' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: off-diagonal only', function t() {
	var out;
	var tc;

	tc = findCase( 'off_diagonal' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: general case', function t() {
	var out;
	var tc;

	tc = findCase( 'general' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: negative diagonal', function t() {
	var out;
	var tc;

	tc = findCase( 'negative_diagonal' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: sm = 0', function t() {
	var out;
	var tc;

	tc = findCase( 'sm_zero' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: equal diagonal', function t() {
	var out;
	var tc;

	tc = findCase( 'equal_diagonal' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: negative off-diagonal', function t() {
	var out;
	var tc;

	tc = findCase( 'negative_offdiag' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: df < 0 (a < c)', function t() {
	var out;
	var tc;

	tc = findCase( 'df_negative' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});

test( 'dlaev2: b=0, a < c', function t() {
	var out;
	var tc;

	tc = findCase( 'b_zero_a_lt_c' );
	out = dlaev2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
	assertClose( out.cs1, tc.cs1, 1e-14, 'cs1' );
	assertClose( out.sn1, tc.sn1, 1e-14, 'sn1' );
});
