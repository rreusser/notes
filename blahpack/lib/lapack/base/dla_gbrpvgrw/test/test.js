/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dla_gbrpvgrw = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dla_gbrpvgrw.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'dla_gbrpvgrw is a function', function t() {
	assert.equal( typeof dla_gbrpvgrw, 'function' );
});

test( 'dla_gbrpvgrw: no_growth', function t() {
	var result;
	var AFB;
	var tc;
	var AB;

	tc = findCase( 'no_growth' );
	AB = new Float64Array( [ 5.0, 3.0, 7.0 ] );
	AFB = new Float64Array( [ 5.0, 3.0, 7.0 ] );
	result = dla_gbrpvgrw( 3, 0, 0, 3, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: ncols_zero', function t() {
	var result;
	var AFB;
	var tc;
	var AB;

	tc = findCase( 'ncols_zero' );
	AB = new Float64Array( [ 5.0, 3.0, 7.0 ] );
	AFB = new Float64Array( [ 5.0, 3.0, 7.0 ] );
	result = dla_gbrpvgrw( 3, 0, 0, 0, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: single_element_growth', function t() {
	var result;
	var AFB;
	var tc;
	var AB;

	tc = findCase( 'single_element_growth' );
	AB = new Float64Array( [ 3.0 ] );
	AFB = new Float64Array( [ 6.0 ] );
	result = dla_gbrpvgrw( 1, 0, 0, 1, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: growth_factor', function t() {
	var result;
	var AFB;
	var tc;
	var AB;

	tc = findCase( 'growth_factor' );
	AB = new Float64Array( [ 2.0, 3.0, 1.0 ] );
	AFB = new Float64Array( [ 4.0, 3.0, 5.0 ] );
	result = dla_gbrpvgrw( 3, 0, 0, 3, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: zero_umax', function t() {
	var result;
	var AFB;
	var tc;
	var AB;

	tc = findCase( 'zero_umax' );
	AB = new Float64Array( [ 5.0, 3.0, 4.0 ] );
	AFB = new Float64Array( [ 5.0, 0.0, 8.0 ] );
	result = dla_gbrpvgrw( 3, 0, 0, 3, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: tridiagonal', function t() {
	var result;
	var AFB;
	var tc;
	var AB;

	tc = findCase( 'tridiagonal' );
	AB = new Float64Array([
		// Col 0: rows [0,1,2]
		0.0,
		4.0,
		1.0,

		// Col 1: rows [0,1,2]
		2.0,
		5.0,
		3.0,

		// Col 2: rows [0,1,2]
		1.0,
		6.0,
		2.0,

		// Col 3: rows [0,1,2]
		4.0,
		3.0,
		0.0
	]);
	AFB = new Float64Array([
		// Col 0: rows [0,1,2,3]
		0.0,
		4.0,
		0.0,
		0.0,

		// Col 1: rows [0,1,2,3]
		2.0,
		10.0,
		0.0,
		0.0,

		// Col 2: rows [0,1,2,3]
		1.0,
		6.0,
		0.0,
		0.0,

		// Col 3: rows [0,1,2,3]
		4.0,
		7.0,
		0.0,
		0.0
	]);
	result = dla_gbrpvgrw( 4, 1, 1, 4, AB, 1, 3, 0, AFB, 1, 4, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: ncols_less_than_n', function t() {
	var result;
	var AFB;
	var tc;
	var AB;

	tc = findCase( 'ncols_less_than_n' );
	AB = new Float64Array( [ 3.0, 4.0, 1.0, 2.0 ] );
	AFB = new Float64Array( [ 6.0, 4.0, 100.0, 100.0 ] );
	result = dla_gbrpvgrw( 4, 0, 0, 2, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: wider_band', function t() {
	var result;
	var AFB;
	var tc;
	var AB;

	tc = findCase( 'wider_band' );
	AB = new Float64Array([
		// Col 0: i=1(row2), i=2(row3), i=3(row4)
		0.0,
		0.0,
		8.0,
		2.0,
		1.0,

		// Col 1: i=1(row1), i=2(row2), i=3(row3), i=4(row4)
		0.0,
		3.0,
		7.0,
		4.0,
		1.0,

		// Col 2: i=1(row0), i=2(row1), i=3(row2), i=4(row3), i=5(row4)
		1.0,
		2.0,
		9.0,
		3.0,
		2.0,

		// Col 3: i=2(row0), i=3(row1), i=4(row2), i=5(row3)
		5.0,
		1.0,
		6.0,
		4.0,
		0.0,

		// Col 4: i=3(row0), i=4(row1), i=5(row2)
		2.0,
		3.0,
		5.0,
		0.0,
		0.0
	]);
	AFB = new Float64Array([
		// Col 0
		0.0,
		0.0,
		8.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 1
		0.0,
		3.0,
		14.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 2
		1.0,
		2.0,
		9.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 3
		5.0,
		1.0,
		12.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 4
		2.0,
		3.0,
		10.0,
		0.0,
		0.0,
		0.0,
		0.0
	]);
	result = dla_gbrpvgrw( 5, 2, 2, 5, AB, 1, 5, 0, AFB, 1, 7, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
