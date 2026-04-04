/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpbstf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zpbstf.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'zpbstf: upper_tridiag_5 (UPLO=U, N=5, KD=1)', function t() {
	var info;
	var tc;
	var AB;

	tc = findCase( 'upper_tridiag_5' );
	AB = new Complex128Array([
		0,
		0,
		4,
		0,
		-1,
		0.5,
		4,
		0,
		-1,
		0.5,
		4,
		0,
		-1,
		0.5,
		4,
		0,
		-1,
		0.5,
		4,
		0
	]);
	info = zpbstf( 'upper', 5, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( AB, 0 ) ), tc.ab, 1e-14, 'AB' );
});

test( 'zpbstf: lower_tridiag_5 (UPLO=L, N=5, KD=1)', function t() {
	var info;
	var tc;
	var AB;

	tc = findCase( 'lower_tridiag_5' );
	AB = new Complex128Array([
		4,
		0,
		-1,
		-0.5,
		4,
		0,
		-1,
		-0.5,
		4,
		0,
		-1,
		-0.5,
		4,
		0,
		-1,
		-0.5,
		4,
		0,
		0,
		0
	]);
	info = zpbstf( 'lower', 5, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( AB, 0 ) ), tc.ab, 1e-14, 'AB' );
});

test( 'zpbstf: upper_penta_4 (UPLO=U, N=4, KD=2)', function t() {
	var info;
	var tc;
	var AB;

	tc = findCase( 'upper_penta_4' );
	AB = new Complex128Array([
		0,
		0,
		0,
		0,
		10,
		0,
		0,
		0,
		1,
		1,
		8,
		0,
		0.5,
		-0.5,
		2,
		0.5,
		6,
		0,
		0,
		0,
		1,
		-0.5,
		7,
		0
	]);
	info = zpbstf( 'upper', 4, 2, AB, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( AB, 0 ) ), tc.ab, 1e-14, 'AB' );
});

test( 'zpbstf: lower_penta_4 (UPLO=L, N=4, KD=2)', function t() {
	var info;
	var tc;
	var AB;

	tc = findCase( 'lower_penta_4' );
	AB = new Complex128Array([
		10,
		0,
		1,
		-1,
		0.5,
		0.5,
		8,
		0,
		2,
		-0.5,
		1,
		0.5,
		6,
		0,
		1,
		-1,
		0,
		0,
		7,
		0,
		0,
		0,
		0,
		0
	]);
	info = zpbstf( 'lower', 4, 2, AB, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( AB, 0 ) ), tc.ab, 1e-14, 'AB' );
});

test( 'zpbstf: n_one (N=1)', function t() {
	var info;
	var tc;
	var AB;

	tc = findCase( 'n_one' );
	AB = new Complex128Array( [ 9, 0 ] );
	info = zpbstf( 'upper', 1, 0, AB, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( AB, 0 ) ), tc.ab, 1e-14, 'AB' );
});

test( 'zpbstf: n_zero (N=0 quick return)', function t() {
	var info;
	var AB;

	AB = new Complex128Array( 4 );
	info = zpbstf( 'upper', 0, 1, AB, 1, 2, 0 );
	assert.equal( info, 0 );
});

test( 'zpbstf: not_hpd_upper (not positive definite, upper)', function t() {
	var info;
	var tc;
	var AB;

	tc = findCase( 'not_hpd_upper' );
	AB = new Complex128Array([
		0,
		0,
		1,
		0,
		2,
		1,
		1,
		0
	]);
	info = zpbstf( 'upper', 2, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
});

test( 'zpbstf: not_hpd_lower (not positive definite, lower)', function t() {
	var info;
	var tc;
	var AB;

	tc = findCase( 'not_hpd_lower' );
	AB = new Complex128Array([
		1,
		0,
		2,
		-1,
		1,
		0,
		0,
		0
	]);
	info = zpbstf( 'lower', 2, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
});

test( 'zpbstf: upper_7x7_kd2 (UPLO=U, N=7, KD=2)', function t() {
	var info;
	var tc;
	var AB;

	tc = findCase( 'upper_7x7_kd2' );
	AB = new Complex128Array([
		0,
		0,
		0,
		0,
		10,
		0,
		0,
		0,
		-1,
		0.5,
		10,
		0,
		0.3,
		-0.2,
		-1,
		0.5,
		10,
		0,
		0.3,
		-0.2,
		-1,
		0.5,
		10,
		0,
		0.3,
		-0.2,
		-1,
		0.5,
		10,
		0,
		0.3,
		-0.2,
		-1,
		0.5,
		10,
		0,
		0.3,
		-0.2,
		-1,
		0.5,
		10,
		0
	]);
	info = zpbstf( 'upper', 7, 2, AB, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( AB, 0 ) ), tc.ab, 1e-14, 'AB' );
});

test( 'zpbstf: lower_7x7_kd2 (UPLO=L, N=7, KD=2)', function t() {
	var info;
	var tc;
	var AB;

	tc = findCase( 'lower_7x7_kd2' );
	AB = new Complex128Array([
		10,
		0,
		-1,
		-0.5,
		0.3,
		0.2,
		10,
		0,
		-1,
		-0.5,
		0.3,
		0.2,
		10,
		0,
		-1,
		-0.5,
		0.3,
		0.2,
		10,
		0,
		-1,
		-0.5,
		0.3,
		0.2,
		10,
		0,
		-1,
		-0.5,
		0.3,
		0.2,
		10,
		0,
		-1,
		-0.5,
		0,
		0,
		10,
		0,
		0,
		0,
		0,
		0
	]);
	info = zpbstf( 'lower', 7, 2, AB, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( AB, 0 ) ), tc.ab, 1e-14, 'AB' );
});
