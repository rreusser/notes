/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarfg = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlarfg.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	if ( relErr > tol ) {
		throw new Error( msg + ': expected ' + expected + ', got ' + actual );
	}
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
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

test( 'dlarfg: basic', function t() {
	var alpha = new Float64Array( [ 3.0 ] );
	var tau = new Float64Array( 1 );
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 4.0, 0.0, 0.0 ] );
	dlarfg( 4, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlarfg: alpha=0', function t() {
	var alpha = new Float64Array( [ 0.0 ] );
	var tau = new Float64Array( 1 );
	var tc = findCase( 'alpha_zero' );
	var x = new Float64Array( [ 3.0, 4.0 ] );
	dlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlarfg: n=1 (tau=0)', function t() {
	var alpha = new Float64Array( [ 5.0 ] );
	var tau = new Float64Array( 1 );
	var tc = findCase( 'n_one' );
	var x = new Float64Array( 1 );
	dlarfg( 1, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
});

test( 'dlarfg: x all zero (tau=0)', function t() {
	var alpha = new Float64Array( [ 5.0 ] );
	var tau = new Float64Array( 1 );
	var tc = findCase( 'x_all_zero' );
	var x = new Float64Array( [ 0.0, 0.0 ] );
	dlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
});

test( 'dlarfg: negative alpha', function t() {
	var alpha = new Float64Array( [ -3.0 ] );
	var tau = new Float64Array( 1 );
	var tc = findCase( 'negative_alpha' );
	var x = new Float64Array( [ 4.0 ] );
	dlarfg( 2, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlarfg: n=0 (tau=0)', function t() {
	var alpha = new Float64Array( [ 5.0 ] );
	var tau = new Float64Array( 1 );
	var tc = findCase( 'n_zero' );
	var x = new Float64Array( 1 );
	dlarfg( 0, alpha, 0, x, 1, 0, tau, 0 );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
});

test( 'dlarfg: tiny inputs (sfmin scaling loop)', function t() {
	var origNorm;
	var alpha;
	var tau;
	var x;

	alpha = new Float64Array( [ 1e-300 ] );
	x = new Float64Array( [ 1e-300, 1e-300 ] );
	tau = new Float64Array( 1 );
	dlarfg( 3, alpha, 0, x, 1, 0, tau, 0 );
	if ( tau[ 0 ] <= 0.0 || tau[ 0 ] > 2.0 ) {
		throw new Error( 'tau out of range: ' + tau[ 0 ] );
	}
	if ( alpha[ 0 ] >= 0.0 ) {
		throw new Error( 'expected negative beta, got: ' + alpha[ 0 ] );
	}
	origNorm = Math.sqrt( 1e-300*1e-300 + 1e-300*1e-300 + 1e-300*1e-300 );
	assertClose( Math.abs( alpha[ 0 ] ), origNorm, 1e-10, 'beta magnitude' );
});

test( 'dlarfg: tiny negative alpha (sfmin scaling loop)', function t() {
	var origNorm;
	var alpha;
	var tau;
	var x;

	alpha = new Float64Array( [ -2e-300 ] );
	x = new Float64Array( [ 1e-300 ] );
	tau = new Float64Array( 1 );
	dlarfg( 2, alpha, 0, x, 1, 0, tau, 0 );
	if ( tau[ 0 ] <= 0.0 || tau[ 0 ] > 2.0 ) {
		throw new Error( 'tau out of range: ' + tau[ 0 ] );
	}
	if ( alpha[ 0 ] <= 0.0 ) {
		throw new Error( 'expected positive beta for negative alpha, got: ' + alpha[ 0 ] ); // eslint-disable-line max-len
	}
	origNorm = Math.sqrt( 4e-600 + 1e-600 );
	assertClose( Math.abs( alpha[ 0 ] ), origNorm, 1e-10, 'beta magnitude' );
});
