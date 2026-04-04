/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqsp = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlaqsp.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

/**
* Maps Fortran single-char EQUED to JS string.
*
* @private
* @param {string} c - Fortran character
* @returns {string} JS equed string
*/
function mapEqued( c ) {
	if ( c === 'Y' ) {
		return 'yes';
	}
	return 'none';
}


// TESTS //

test( 'dlaqsp: upper_equilibrate', function t() {
	var equed;
	var tc;
	var AP;
	var S;

	tc = findCase( 'upper_equilibrate' );
	AP = new Float64Array( [ 4.0, 1.0, 9.0, 0.5, 2.0, 16.0 ] );
	S = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );
	equed = dlaqsp( 'upper', 3, AP, 1, 0, S, 1, 0, 0.05, 16.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( AP ), tc.ap, 1e-14, 'ap' );
});

test( 'dlaqsp: lower_equilibrate', function t() {
	var equed;
	var tc;
	var AP;
	var S;

	tc = findCase( 'lower_equilibrate' );
	AP = new Float64Array( [ 4.0, 1.0, 0.5, 9.0, 2.0, 16.0 ] );
	S = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );
	equed = dlaqsp( 'lower', 3, AP, 1, 0, S, 1, 0, 0.05, 16.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( AP ), tc.ap, 1e-14, 'ap' );
});

test( 'dlaqsp: no_equilibrate', function t() {
	var equed;
	var tc;
	var AP;
	var S;

	tc = findCase( 'no_equilibrate' );
	AP = new Float64Array( [ 4.0, 1.0, 9.0, 0.5, 2.0, 16.0 ] );
	S = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	equed = dlaqsp( 'upper', 3, AP, 1, 0, S, 1, 0, 0.5, 16.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( AP ), tc.ap, 1e-14, 'ap' );
});

test( 'dlaqsp: n_zero', function t() {
	var equed;
	var tc;
	var AP;
	var S;

	tc = findCase( 'n_zero' );
	AP = new Float64Array( 1 );
	S = new Float64Array( 1 );
	equed = dlaqsp( 'upper', 0, AP, 1, 0, S, 1, 0, 1.0, 1.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
});

test( 'dlaqsp: n_one_upper', function t() {
	var equed;
	var tc;
	var AP;
	var S;

	tc = findCase( 'n_one_upper' );
	AP = new Float64Array( [ 100.0 ] );
	S = new Float64Array( [ 0.1 ] );
	equed = dlaqsp( 'upper', 1, AP, 1, 0, S, 1, 0, 0.01, 100.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( AP ), tc.ap, 1e-14, 'ap' );
});

test( 'dlaqsp: small_amax', function t() {
	var equed;
	var tc;
	var AP;
	var S;

	tc = findCase( 'small_amax' );
	AP = new Float64Array( [ 1e-300, 0.0, 1e-300 ] );
	S = new Float64Array( [ 1e150, 1e150 ] );
	equed = dlaqsp( 'upper', 2, AP, 1, 0, S, 1, 0, 1.0, 1e-300 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( AP ), tc.ap, 1e-14, 'ap' );
});

test( 'dlaqsp: upper_4x4', function t() {
	var equed;
	var tc;
	var AP;
	var S;

	tc = findCase( 'upper_4x4' );
	AP = new Float64Array( [ 16.0, 4.0, 9.0, 2.0, 3.0, 25.0, 1.0, 6.0, 5.0, 36.0 ] ); // eslint-disable-line max-len
	S = new Float64Array( [ 0.25, 0.5, 0.2, 1.0/3.0 ] );
	equed = dlaqsp( 'upper', 4, AP, 1, 0, S, 1, 0, 0.01, 36.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( AP ), tc.ap, 1e-14, 'ap' );
});

test( 'dlaqsp: lower_4x4', function t() {
	var equed;
	var tc;
	var AP;
	var S;

	tc = findCase( 'lower_4x4' );
	AP = new Float64Array( [ 16.0, 4.0, 2.0, 1.0, 9.0, 3.0, 6.0, 25.0, 5.0, 36.0 ] ); // eslint-disable-line max-len
	S = new Float64Array( [ 0.25, 0.5, 0.2, 1.0/3.0 ] );
	equed = dlaqsp( 'lower', 4, AP, 1, 0, S, 1, 0, 0.01, 36.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( AP ), tc.ap, 1e-14, 'ap' );
});

test( 'dlaqsp: main export is a function', function t() {
	var main = require( './../lib' );
	assert.strictEqual( typeof main, 'function' );
});

test( 'dlaqsp: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	var main = require( './../lib' );
	assert.strictEqual( typeof main.ndarray, 'function' );
});

test( 'dlaqsp: standard API works', function t() {
	var equed;
	var main;
	var AP;
	var S;

	main = require( './../lib' );
	AP = new Float64Array( [ 4.0, 1.0, 9.0, 0.5, 2.0, 16.0 ] );
	S = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );
	equed = main( 'upper', 3, AP, S, 1, 0.05, 16.0 );
	assert.equal( equed, 'yes' );
});

test( 'dlaqsp: ndarray API works', function t() {
	var equed;
	var main;
	var AP;
	var S;

	main = require( './../lib' );
	AP = new Float64Array( [ 4.0, 1.0, 9.0, 0.5, 2.0, 16.0 ] );
	S = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );
	equed = main.ndarray( 'upper', 3, AP, 1, 0, S, 1, 0, 0.05, 16.0 );
	assert.equal( equed, 'yes' );
});

test( 'dlaqsp: ndarray API validates uplo', function t() {
	var main;
	var AP;
	var S;

	main = require( './../lib' );
	AP = new Float64Array( 6 );
	S = new Float64Array( 3 );
	assert.throws( function badUplo() {
		main.ndarray( 'invalid', 3, AP, 1, 0, S, 1, 0, 0.5, 1.0 );
	}, /invalid argument/ );
});

test( 'dlaqsp: standard API validates uplo', function t() {
	var main;
	var AP;
	var S;

	main = require( './../lib' );
	AP = new Float64Array( 6 );
	S = new Float64Array( 3 );
	assert.throws( function badUplo() {
		main( 'invalid', 3, AP, S, 1, 0.5, 1.0 );
	}, /invalid argument/ );
});
