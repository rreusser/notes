/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var dlapy2 = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlapy2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );

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

test( 'dlapy2: basic 3,4 -> 5', function t() {
	var tc = findCase( 'basic' );
	assertClose( dlapy2( 3.0, 4.0 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: x=0', function t() {
	var tc = findCase( 'x_zero' );
	assertClose( dlapy2( 0.0, 5.0 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: y=0', function t() {
	var tc = findCase( 'y_zero' );
	assertClose( dlapy2( 7.0, 0.0 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: both zero', function t() {
	var tc = findCase( 'both_zero' );
	assert.strictEqual( dlapy2( 0.0, 0.0 ), tc.result );
});

test( 'dlapy2: large values', function t() {
	var tc = findCase( 'large' );
	assertClose( dlapy2( 1e+154, 1e+154 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: negative values', function t() {
	var tc = findCase( 'negative' );
	assertClose( dlapy2( -3.0, -4.0 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: NaN propagation (x=NaN)', function t() {
	var result = dlapy2( NaN, 3.0 );
	assert.ok( result !== result, 'expected NaN' );
});

test( 'dlapy2: NaN propagation (y=NaN)', function t() {
	var result = dlapy2( 3.0, NaN );
	assert.ok( result !== result, 'expected NaN' );
});
