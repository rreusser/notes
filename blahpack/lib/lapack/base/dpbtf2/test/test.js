/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpbtf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dpbtf2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'dpbtf2: upper_tridiag_5', function t() {
	var info;
	var tc;
	var ab;

	tc = findCase( 'upper_tridiag_5' );
	ab = new Float64Array([
		0.0,
		2.0,   // col 0
		-1.0,
		2.0,   // col 1
		-1.0,
		2.0,   // col 2
		-1.0,
		2.0,   // col 3
		-1.0,
		2.0    // col 4
	]);
	info = dpbtf2( 'upper', 5, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: lower_tridiag_5', function t() {
	var info;
	var tc;
	var ab;

	tc = findCase( 'lower_tridiag_5' );
	ab = new Float64Array([
		2.0,
		-1.0,  // col 0
		2.0,
		-1.0,  // col 1
		2.0,
		-1.0,  // col 2
		2.0,
		-1.0,  // col 3
		2.0,
		0.0   // col 4
	]);
	info = dpbtf2( 'lower', 5, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: upper_penta_4', function t() {
	var info;
	var tc;
	var ab;

	tc = findCase( 'upper_penta_4' );
	ab = new Float64Array([
		0.0,
		0.0,
		4.0,   // col 0
		0.0,
		-1.0,
		4.0,   // col 1
		0.5,
		-1.0,
		4.0,   // col 2
		0.5,
		-1.0,
		4.0    // col 3
	]);
	info = dpbtf2( 'upper', 4, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: lower_penta_4', function t() {
	var info;
	var tc;
	var ab;

	tc = findCase( 'lower_penta_4' );
	ab = new Float64Array([
		4.0,
		-1.0,
		0.5,   // col 0
		4.0,
		-1.0,
		0.5,   // col 1
		4.0,
		-1.0,
		0.0,   // col 2
		4.0,
		0.0,
		0.0    // col 3
	]);
	info = dpbtf2( 'lower', 4, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: n_one', function t() {
	var info;
	var tc;
	var ab;

	tc = findCase( 'n_one' );
	ab = new Float64Array([ 9.0 ]);
	info = dpbtf2( 'upper', 1, 0, ab, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: n_zero', function t() {
	var info;
	var tc;
	var ab;

	tc = findCase( 'n_zero' );
	ab = new Float64Array([ 99.0 ]);
	info = dpbtf2( 'lower', 0, 0, ab, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpbtf2: not_posdef', function t() {
	var info;
	var tc;
	var ab;

	tc = findCase( 'not_posdef' );
	ab = new Float64Array([
		1.0,
		2.0,   // col 0
		1.0,
		0.0    // col 1
	]);
	info = dpbtf2( 'lower', 2, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: upper_full_3', function t() {
	var info;
	var tc;
	var ab;

	tc = findCase( 'upper_full_3' );
	ab = new Float64Array([
		0.0,
		0.0,
		4.0,   // col 0
		0.0,
		2.0,
		5.0,   // col 1
		1.0,
		3.0,
		6.0    // col 2
	]);
	info = dpbtf2( 'upper', 3, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: not_posdef upper', function t() {
	var info;
	var ab;

	ab = new Float64Array([
		0.0,
		1.0,   // col 0: diag=1
		2.0,
		1.0    // col 1: superdiag=2, diag=1
	]);
	info = dpbtf2( 'upper', 2, 1, ab, 1, 2, 0 );
	assert.equal( info, 2 );
});
