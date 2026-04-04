/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlaqhb = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zlaqhb.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len, node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// VARIABLES //

var LDAB = 5;


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

test( 'zlaqhb is a function', function t() {
	assert.equal( typeof zlaqhb, 'function' );
});

test( 'zlaqhb: upper_kd1 - upper band matrix with KD=1, equilibration needed (Hermitian diag zeroing)', function t() { // eslint-disable-line max-len
	var equed;
	var tc;
	var AB;
	var Av;
	var S;
	var N;

	tc = findCase( 'upper_kd1' );
	N = 4;
	AB = new Complex128Array( LDAB * N );
	Av = reinterpret( AB, 0 );
	Av[ 2*(1 + (0 * LDAB)) ] = 4.0;
	Av[ 2*(1 + (0 * LDAB)) + 1 ] = 0.1;
	Av[ 2*(1 + (1 * LDAB)) ] = 9.0;
	Av[ 2*(1 + (1 * LDAB)) + 1 ] = 0.2;
	Av[ 2*(1 + (2 * LDAB)) ] = 16.0;
	Av[ 2*(1 + (2 * LDAB)) + 1 ] = 0.3;
	Av[ 2*(1 + (3 * LDAB)) ] = 25.0;
	Av[ 2*(1 + (3 * LDAB)) + 1 ] = 0.4;
	Av[ 2*(0 + (1 * LDAB)) ] = 1.0;
	Av[ 2*(0 + (1 * LDAB)) + 1 ] = 2.0;
	Av[ 2*(0 + (2 * LDAB)) ] = 3.0;
	Av[ 2*(0 + (2 * LDAB)) + 1 ] = 4.0;
	Av[ 2*(0 + (3 * LDAB)) ] = 5.0;
	Av[ 2*(0 + (3 * LDAB)) + 1 ] = 6.0;
	S = new Float64Array( [ 0.5, 0.25, 0.2, 0.1 ] );
	equed = zlaqhb( 'upper', N, 1, AB, 1, LDAB, 0, S, 1, 0, 0.02, 25.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( Av ), tc.ab, 1e-14, 'ab' );
});

test( 'zlaqhb: lower_kd1 - lower band matrix with KD=1, equilibration needed (Hermitian diag zeroing)', function t() { // eslint-disable-line max-len
	var equed;
	var tc;
	var AB;
	var Av;
	var S;
	var N;

	tc = findCase( 'lower_kd1' );
	N = 4;
	AB = new Complex128Array( LDAB * N );
	Av = reinterpret( AB, 0 );
	Av[ 2*(0 + (0 * LDAB)) ] = 4.0;
	Av[ 2*(0 + (0 * LDAB)) + 1 ] = 0.1;
	Av[ 2*(0 + (1 * LDAB)) ] = 9.0;
	Av[ 2*(0 + (1 * LDAB)) + 1 ] = 0.2;
	Av[ 2*(0 + (2 * LDAB)) ] = 16.0;
	Av[ 2*(0 + (2 * LDAB)) + 1 ] = 0.3;
	Av[ 2*(0 + (3 * LDAB)) ] = 25.0;
	Av[ 2*(0 + (3 * LDAB)) + 1 ] = 0.4;
	Av[ 2*(1 + (0 * LDAB)) ] = 1.0;
	Av[ 2*(1 + (0 * LDAB)) + 1 ] = 2.0;
	Av[ 2*(1 + (1 * LDAB)) ] = 3.0;
	Av[ 2*(1 + (1 * LDAB)) + 1 ] = 4.0;
	Av[ 2*(1 + (2 * LDAB)) ] = 5.0;
	Av[ 2*(1 + (2 * LDAB)) + 1 ] = 6.0;
	S = new Float64Array( [ 0.5, 0.25, 0.2, 0.1 ] );
	equed = zlaqhb( 'lower', N, 1, AB, 1, LDAB, 0, S, 1, 0, 0.02, 25.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( Av ), tc.ab, 1e-14, 'ab' );
});

test( 'zlaqhb: no_equilibrate - good scond, amax in range', function t() {
	var equed;
	var tc;
	var AB;
	var Av;
	var S;
	var N;

	tc = findCase( 'no_equilibrate' );
	N = 4;
	AB = new Complex128Array( LDAB * N );
	Av = reinterpret( AB, 0 );
	Av[ 2*(1 + (0 * LDAB)) ] = 4.0;
	Av[ 2*(1 + (0 * LDAB)) + 1 ] = 0.1;
	Av[ 2*(1 + (1 * LDAB)) ] = 9.0;
	Av[ 2*(1 + (1 * LDAB)) + 1 ] = 0.2;
	Av[ 2*(1 + (2 * LDAB)) ] = 16.0;
	Av[ 2*(1 + (2 * LDAB)) + 1 ] = 0.3;
	Av[ 2*(1 + (3 * LDAB)) ] = 25.0;
	Av[ 2*(1 + (3 * LDAB)) + 1 ] = 0.4;
	Av[ 2*(0 + (1 * LDAB)) ] = 1.0;
	Av[ 2*(0 + (1 * LDAB)) + 1 ] = 2.0;
	Av[ 2*(0 + (2 * LDAB)) ] = 3.0;
	Av[ 2*(0 + (2 * LDAB)) + 1 ] = 4.0;
	Av[ 2*(0 + (3 * LDAB)) ] = 5.0;
	Av[ 2*(0 + (3 * LDAB)) + 1 ] = 6.0;
	S = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	equed = zlaqhb( 'upper', N, 1, AB, 1, LDAB, 0, S, 1, 0, 0.5, 25.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( Av ), tc.ab, 1e-14, 'ab' );
});

test( 'zlaqhb: n_zero - quick return', function t() {
	var equed;
	var tc;
	var AB;
	var S;

	tc = findCase( 'n_zero' );
	AB = new Complex128Array( 1 );
	S = new Float64Array( 1 );
	equed = zlaqhb( 'upper', 0, 1, AB, 1, LDAB, 0, S, 1, 0, 0.5, 25.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
});

test( 'zlaqhb: n_one_upper - single element with imaginary part, equilibration needed', function t() { // eslint-disable-line max-len
	var equed;
	var tc;
	var AB;
	var Av;
	var S;

	tc = findCase( 'n_one_upper' );
	AB = new Complex128Array( LDAB * 1 );
	Av = reinterpret( AB, 0 );
	Av[ 0 ] = 100.0;
	Av[ 1 ] = 7.5;
	S = new Float64Array( [ 0.1 ] );
	equed = zlaqhb( 'upper', 1, 0, AB, 1, LDAB, 0, S, 1, 0, 0.01, 100.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( Av ), tc.ab, 1e-14, 'ab' );
});

test( 'zlaqhb: upper_kd2 - wider bandwidth KD=2, upper', function t() {
	var equed;
	var tc;
	var AB;
	var Av;
	var S;
	var N;

	tc = findCase( 'upper_kd2' );
	N = 4;
	AB = new Complex128Array( LDAB * N );
	Av = reinterpret( AB, 0 );
	Av[ 2*(2 + (0 * LDAB)) ] = 10.0;
	Av[ 2*(2 + (0 * LDAB)) + 1 ] = 0.5;
	Av[ 2*(2 + (1 * LDAB)) ] = 20.0;
	Av[ 2*(2 + (1 * LDAB)) + 1 ] = 0.7;
	Av[ 2*(2 + (2 * LDAB)) ] = 30.0;
	Av[ 2*(2 + (2 * LDAB)) + 1 ] = 0.9;
	Av[ 2*(2 + (3 * LDAB)) ] = 40.0;
	Av[ 2*(2 + (3 * LDAB)) + 1 ] = 1.1;
	Av[ 2*(1 + (1 * LDAB)) ] = 1.0;
	Av[ 2*(1 + (1 * LDAB)) + 1 ] = 0.5;
	Av[ 2*(1 + (2 * LDAB)) ] = 2.0;
	Av[ 2*(1 + (2 * LDAB)) + 1 ] = 1.0;
	Av[ 2*(1 + (3 * LDAB)) ] = 3.0;
	Av[ 2*(1 + (3 * LDAB)) + 1 ] = 1.5;
	Av[ 2*(0 + (2 * LDAB)) ] = 0.5;
	Av[ 2*(0 + (2 * LDAB)) + 1 ] = 0.25;
	Av[ 2*(0 + (3 * LDAB)) ] = 1.5;
	Av[ 2*(0 + (3 * LDAB)) + 1 ] = 0.75;
	S = new Float64Array( [ 0.5, 0.25, 0.2, 0.1 ] );
	equed = zlaqhb( 'upper', N, 2, AB, 1, LDAB, 0, S, 1, 0, 0.02, 40.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( Av ), tc.ab, 1e-14, 'ab' );
});

test( 'zlaqhb: lower_kd2 - wider bandwidth KD=2, lower', function t() {
	var equed;
	var tc;
	var AB;
	var Av;
	var S;
	var N;

	tc = findCase( 'lower_kd2' );
	N = 4;
	AB = new Complex128Array( LDAB * N );
	Av = reinterpret( AB, 0 );
	Av[ 2*(0 + (0 * LDAB)) ] = 10.0;
	Av[ 2*(0 + (0 * LDAB)) + 1 ] = 0.5;
	Av[ 2*(0 + (1 * LDAB)) ] = 20.0;
	Av[ 2*(0 + (1 * LDAB)) + 1 ] = 0.7;
	Av[ 2*(0 + (2 * LDAB)) ] = 30.0;
	Av[ 2*(0 + (2 * LDAB)) + 1 ] = 0.9;
	Av[ 2*(0 + (3 * LDAB)) ] = 40.0;
	Av[ 2*(0 + (3 * LDAB)) + 1 ] = 1.1;
	Av[ 2*(1 + (0 * LDAB)) ] = 1.0;
	Av[ 2*(1 + (0 * LDAB)) + 1 ] = 0.5;
	Av[ 2*(1 + (1 * LDAB)) ] = 2.0;
	Av[ 2*(1 + (1 * LDAB)) + 1 ] = 1.0;
	Av[ 2*(1 + (2 * LDAB)) ] = 3.0;
	Av[ 2*(1 + (2 * LDAB)) + 1 ] = 1.5;
	Av[ 2*(2 + (0 * LDAB)) ] = 0.5;
	Av[ 2*(2 + (0 * LDAB)) + 1 ] = 0.25;
	Av[ 2*(2 + (1 * LDAB)) ] = 1.5;
	Av[ 2*(2 + (1 * LDAB)) + 1 ] = 0.75;
	S = new Float64Array( [ 0.5, 0.25, 0.2, 0.1 ] );
	equed = zlaqhb( 'lower', N, 2, AB, 1, LDAB, 0, S, 1, 0, 0.02, 40.0 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( Av ), tc.ab, 1e-14, 'ab' );
});

test( 'zlaqhb: small_amax - amax very small triggers equilibration', function t() { // eslint-disable-line max-len
	var equed;
	var tc;
	var AB;
	var Av;
	var S;
	var N;

	tc = findCase( 'small_amax' );
	N = 2;
	AB = new Complex128Array( LDAB * N );
	Av = reinterpret( AB, 0 );
	Av[ 2*(1 + (0 * LDAB)) ] = 1.0e-300;
	Av[ 2*(1 + (0 * LDAB)) + 1 ] = 0.5e-300;
	Av[ 2*(0 + (1 * LDAB)) ] = 0.0;
	Av[ 2*(1 + (1 * LDAB)) ] = 1.0e-300;
	Av[ 2*(1 + (1 * LDAB)) + 1 ] = 0.3e-300;
	S = new Float64Array( [ 1.0e150, 1.0e150 ] );
	equed = zlaqhb( 'upper', N, 1, AB, 1, LDAB, 0, S, 1, 0, 1.0, 1.0e-300 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( Av ), tc.ab, 1e-14, 'ab' );
});

test( 'zlaqhb: large_amax - amax very large triggers equilibration', function t() { // eslint-disable-line max-len
	var equed;
	var tc;
	var AB;
	var Av;
	var S;
	var N;

	tc = findCase( 'large_amax' );
	N = 2;
	AB = new Complex128Array( LDAB * N );
	Av = reinterpret( AB, 0 );
	Av[ 2*(1 + (0 * LDAB)) ] = 1.0e300;
	Av[ 2*(1 + (0 * LDAB)) + 1 ] = 0.5e300;
	Av[ 2*(0 + (1 * LDAB)) ] = 0.0;
	Av[ 2*(1 + (1 * LDAB)) ] = 1.0e300;
	Av[ 2*(1 + (1 * LDAB)) + 1 ] = 0.3e300;
	S = new Float64Array( [ 1.0e-150, 1.0e-150 ] );
	equed = zlaqhb( 'upper', N, 1, AB, 1, LDAB, 0, S, 1, 0, 1.0, 1.0e300 );
	assert.equal( equed, mapEqued( tc.equed ), 'equed' );
	assertArrayClose( toArray( Av ), tc.ab, 1e-14, 'ab' );
});
