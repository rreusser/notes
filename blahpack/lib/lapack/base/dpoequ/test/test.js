/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpoequ = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dpoequ.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'dpoequ: basic', function t() {
	var result;
	var tc;
	var A;
	var s;

	tc = findCase( 'basic' );
	A = new Float64Array([ 4.0, 1.0, 0.5, 1.0, 9.0, 1.0, 0.5, 1.0, 16.0 ]);
	s = new Float64Array( 3 );
	result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( toArray( s ), tc.s, 1e-14, 's' );
});

test( 'dpoequ: diagonal_varied', function t() {
	var result;
	var tc;
	var A;
	var s;

	tc = findCase( 'diagonal_varied' );
	A = new Float64Array([ 100.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.25 ]);
	s = new Float64Array( 3 );
	result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( toArray( s ), tc.s, 1e-14, 's' );
});

test( 'dpoequ: non_positive_diag', function t() {
	var result;
	var tc;
	var A;
	var s;

	tc = findCase( 'non_positive_diag' );
	A = new Float64Array([ 4.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 0.0, 9.0 ]);
	s = new Float64Array( 3 );
	result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dpoequ: zero_diag', function t() {
	var result;
	var tc;
	var A;
	var s;

	tc = findCase( 'zero_diag' );
	A = new Float64Array([ 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 9.0 ]);
	s = new Float64Array( 3 );
	result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dpoequ: n_zero', function t() {
	var result;
	var tc;
	var A;
	var s;

	tc = findCase( 'n_zero' );
	A = new Float64Array( 1 );
	s = new Float64Array( 1 );
	result = dpoequ( 0, A, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dpoequ: n_one', function t() {
	var result;
	var tc;
	var A;
	var s;

	tc = findCase( 'n_one' );
	A = new Float64Array([ 25.0 ]);
	s = new Float64Array( 1 );
	result = dpoequ( 1, A, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( toArray( s ), tc.s, 1e-14, 's' );
});

test( 'dpoequ: identity', function t() {
	var result;
	var tc;
	var A;
	var s;

	tc = findCase( 'identity' );
	A = new Float64Array([ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ]);
	s = new Float64Array( 3 );
	result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( toArray( s ), tc.s, 1e-14, 's' );
});
