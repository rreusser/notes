/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len, no-lonely-if, no-unused-vars, no-mixed-operators, require-jsdoc, max-lines, stdlib/jsdoc-private-annotation, max-statements-per-line */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlansf = require( './../lib/base.js' );


// FIXTURES //

var even4_nl_frob = require( './fixtures/even4_nl_frob.json' );
var even4_nl_inf = require( './fixtures/even4_nl_inf.json' );
var even4_nl_max = require( './fixtures/even4_nl_max.json' );
var even4_nl_one = require( './fixtures/even4_nl_one.json' );
var even4_nu_frob = require( './fixtures/even4_nu_frob.json' );
var even4_nu_inf = require( './fixtures/even4_nu_inf.json' );
var even4_nu_max = require( './fixtures/even4_nu_max.json' );
var even4_nu_one = require( './fixtures/even4_nu_one.json' );
var even4_tl_frob = require( './fixtures/even4_tl_frob.json' );
var even4_tl_inf = require( './fixtures/even4_tl_inf.json' );
var even4_tl_max = require( './fixtures/even4_tl_max.json' );
var even4_tl_one = require( './fixtures/even4_tl_one.json' );
var even4_tu_frob = require( './fixtures/even4_tu_frob.json' );
var even4_tu_inf = require( './fixtures/even4_tu_inf.json' );
var even4_tu_max = require( './fixtures/even4_tu_max.json' );
var even4_tu_one = require( './fixtures/even4_tu_one.json' );
var n_one_frob = require( './fixtures/n_one_frob.json' );
var n_one_max = require( './fixtures/n_one_max.json' );
var n_one_one = require( './fixtures/n_one_one.json' );
var n_zero_frob = require( './fixtures/n_zero_frob.json' );
var n_zero_max = require( './fixtures/n_zero_max.json' );
var n_zero_one = require( './fixtures/n_zero_one.json' );
var odd5_nl_frob = require( './fixtures/odd5_nl_frob.json' );
var odd5_nl_inf = require( './fixtures/odd5_nl_inf.json' );
var odd5_nl_max = require( './fixtures/odd5_nl_max.json' );
var odd5_nl_one = require( './fixtures/odd5_nl_one.json' );
var odd5_nu_frob = require( './fixtures/odd5_nu_frob.json' );
var odd5_nu_inf = require( './fixtures/odd5_nu_inf.json' );
var odd5_nu_max = require( './fixtures/odd5_nu_max.json' );
var odd5_nu_one = require( './fixtures/odd5_nu_one.json' );
var odd5_tl_frob = require( './fixtures/odd5_tl_frob.json' );
var odd5_tl_inf = require( './fixtures/odd5_tl_inf.json' );
var odd5_tl_max = require( './fixtures/odd5_tl_max.json' );
var odd5_tl_one = require( './fixtures/odd5_tl_one.json' );
var odd5_tu_frob = require( './fixtures/odd5_tu_frob.json' );
var odd5_tu_inf = require( './fixtures/odd5_tu_inf.json' );
var odd5_tu_max = require( './fixtures/odd5_tu_max.json' );
var odd5_tu_one = require( './fixtures/odd5_tu_one.json' );
var odd7_nl_frob = require( './fixtures/odd7_nl_frob.json' );
var odd7_nl_inf = require( './fixtures/odd7_nl_inf.json' );
var odd7_nl_max = require( './fixtures/odd7_nl_max.json' );
var odd7_nl_one = require( './fixtures/odd7_nl_one.json' );
var odd7_nu_frob = require( './fixtures/odd7_nu_frob.json' );
var odd7_nu_inf = require( './fixtures/odd7_nu_inf.json' );
var odd7_nu_max = require( './fixtures/odd7_nu_max.json' );
var odd7_nu_one = require( './fixtures/odd7_nu_one.json' );
var odd7_tl_frob = require( './fixtures/odd7_tl_frob.json' );
var odd7_tl_inf = require( './fixtures/odd7_tl_inf.json' );
var odd7_tl_max = require( './fixtures/odd7_tl_max.json' );
var odd7_tl_one = require( './fixtures/odd7_tl_one.json' );
var odd7_tu_frob = require( './fixtures/odd7_tu_frob.json' );
var odd7_tu_inf = require( './fixtures/odd7_tu_inf.json' );
var odd7_tu_max = require( './fixtures/odd7_tu_max.json' );
var odd7_tu_one = require( './fixtures/odd7_tu_one.json' );

var fixtures = {
	'even4_NL_frob': even4_nl_frob,
	'even4_NL_inf': even4_nl_inf,
	'even4_NL_max': even4_nl_max,
	'even4_NL_one': even4_nl_one,
	'even4_NU_frob': even4_nu_frob,
	'even4_NU_inf': even4_nu_inf,
	'even4_NU_max': even4_nu_max,
	'even4_NU_one': even4_nu_one,
	'even4_TL_frob': even4_tl_frob,
	'even4_TL_inf': even4_tl_inf,
	'even4_TL_max': even4_tl_max,
	'even4_TL_one': even4_tl_one,
	'even4_TU_frob': even4_tu_frob,
	'even4_TU_inf': even4_tu_inf,
	'even4_TU_max': even4_tu_max,
	'even4_TU_one': even4_tu_one,
	'n_one_frob': n_one_frob,
	'n_one_max': n_one_max,
	'n_one_one': n_one_one,
	'n_zero_frob': n_zero_frob,
	'n_zero_max': n_zero_max,
	'n_zero_one': n_zero_one,
	'odd5_NL_frob': odd5_nl_frob,
	'odd5_NL_inf': odd5_nl_inf,
	'odd5_NL_max': odd5_nl_max,
	'odd5_NL_one': odd5_nl_one,
	'odd5_NU_frob': odd5_nu_frob,
	'odd5_NU_inf': odd5_nu_inf,
	'odd5_NU_max': odd5_nu_max,
	'odd5_NU_one': odd5_nu_one,
	'odd5_TL_frob': odd5_tl_frob,
	'odd5_TL_inf': odd5_tl_inf,
	'odd5_TL_max': odd5_tl_max,
	'odd5_TL_one': odd5_tl_one,
	'odd5_TU_frob': odd5_tu_frob,
	'odd5_TU_inf': odd5_tu_inf,
	'odd5_TU_max': odd5_tu_max,
	'odd5_TU_one': odd5_tu_one,
	'odd7_NL_frob': odd7_nl_frob,
	'odd7_NL_inf': odd7_nl_inf,
	'odd7_NL_max': odd7_nl_max,
	'odd7_NL_one': odd7_nl_one,
	'odd7_NU_frob': odd7_nu_frob,
	'odd7_NU_inf': odd7_nu_inf,
	'odd7_NU_max': odd7_nu_max,
	'odd7_NU_one': odd7_nu_one,
	'odd7_TL_frob': odd7_tl_frob,
	'odd7_TL_inf': odd7_tl_inf,
	'odd7_TL_max': odd7_tl_max,
	'odd7_TL_one': odd7_tl_one,
	'odd7_TU_frob': odd7_tu_frob,
	'odd7_TU_inf': odd7_tu_inf,
	'odd7_TU_max': odd7_tu_max,
	'odd7_TU_one': odd7_tu_one
};


// FUNCTIONS //

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr;
	if ( expected === 0.0 ) {
		assert.ok( Math.abs( actual ) <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
		return;
	}
	relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Parses the norm type from a fixture name.
*
* @private
* @param {string} name - fixture name
* @returns {string} norm string for dlansf
*/
function parseNorm( name ) {
	if ( name.indexOf( '_max' ) !== -1 ) {
		return 'max';
	}
	if ( name.indexOf( '_one' ) !== -1 ) {
		return 'one-norm';
	}
	if ( name.indexOf( '_inf' ) !== -1 ) {
		return 'inf-norm';
	}
	if ( name.indexOf( '_frob' ) !== -1 ) {
		return 'frobenius';
	}
	return 'max';
}

/**
* Parses the transr type from a fixture name.
*
* @private
* @param {string} name - fixture name
* @returns {string} transr string for dlansf
*/
function parseTransr( name ) {
	// Pattern: prefix_XY_norm where X is N or T
	var parts = name.split( '_' );
	var combo;
	var i;
	for ( i = 0; i < parts.length; i++ ) {
		if ( parts[ i ].length === 2 && ( parts[ i ][ 0 ] === 'N' || parts[ i ][ 0 ] === 'T' ) && ( parts[ i ][ 1 ] === 'L' || parts[ i ][ 1 ] === 'U' ) ) { // eslint-disable-line max-len
			combo = parts[ i ];
			break;
		}
	}
	if ( !combo ) {
		return 'no-transpose';
	}
	return ( combo[ 0 ] === 'T' ) ? 'transpose' : 'no-transpose';
}

/**
* Parses the uplo type from a fixture name.
*
* @private
* @param {string} name - fixture name
* @returns {string} uplo string for dlansf
*/
function parseUplo( name ) {
	var parts = name.split( '_' );
	var combo;
	var i;
	for ( i = 0; i < parts.length; i++ ) {
		if ( parts[ i ].length === 2 && ( parts[ i ][ 0 ] === 'N' || parts[ i ][ 0 ] === 'T' ) && ( parts[ i ][ 1 ] === 'L' || parts[ i ][ 1 ] === 'U' ) ) { // eslint-disable-line max-len
			combo = parts[ i ];
			break;
		}
	}
	if ( !combo ) {
		return 'lower';
	}
	return ( combo[ 1 ] === 'U' ) ? 'upper' : 'lower';
}

/**
* Parses N from a fixture name.
*
* @private
* @param {string} name - fixture name
* @returns {number} matrix order N
*/
function parseN( name ) {
	if ( name.indexOf( 'odd5' ) !== -1 ) {
		return 5;
	}
	if ( name.indexOf( 'odd7' ) !== -1 ) {
		return 7;
	}
	if ( name.indexOf( 'even4' ) !== -1 ) {
		return 4;
	}
	if ( name.indexOf( 'n_zero' ) !== -1 ) {
		return 0;
	}
	if ( name.indexOf( 'n_one' ) !== -1 ) {
		return 1;
	}
	return 0;
}

/**
* Runs a single test case from the fixture.
*
* @private
* @param {string} name - fixture case name
*/
function runTest( name ) {
	test( 'dlansf: ' + name, function t() {
		var transr;
		var result;
		var WORK;
		var norm;
		var uplo;
		var tc;
		var N;
		var A;

		tc = fixtures[ name ];
		N = parseN( name );
		norm = parseNorm( name );
		transr = parseTransr( name );
		uplo = parseUplo( name );
		A = new Float64Array( tc.input );
		WORK = new Float64Array( Math.max( N, 1 ) );
		result = dlansf( norm, transr, uplo, N, A, 1, 0, WORK, 1, 0 );
		assertClose( result, tc.result, 1e-14, name );
	});
}


// TESTS //

test( 'dlansf is a function', function t() {
	assert.strictEqual( typeof dlansf, 'function' );
});

test( 'dlansf returns 0 for N=0', function t() {
	var A = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	assert.strictEqual( dlansf( 'max', 'no-transpose', 'lower', 0, A, 1, 0, WORK, 1, 0 ), 0.0 );
	assert.strictEqual( dlansf( 'one-norm', 'no-transpose', 'lower', 0, A, 1, 0, WORK, 1, 0 ), 0.0 );
	assert.strictEqual( dlansf( 'frobenius', 'no-transpose', 'lower', 0, A, 1, 0, WORK, 1, 0 ), 0.0 );
});

test( 'dlansf returns |A(0)| for N=1', function t() {
	var A = new Float64Array( [ -7.5 ] );
	var WORK = new Float64Array( 1 );
	assert.strictEqual( dlansf( 'max', 'no-transpose', 'lower', 1, A, 1, 0, WORK, 1, 0 ), 7.5 );
});

// All 8 dispatch paths x 4 norms for N=5 (odd)
runTest( 'odd5_NL_max' );
runTest( 'odd5_NL_one' );
runTest( 'odd5_NL_inf' );
runTest( 'odd5_NL_frob' );
runTest( 'odd5_NU_max' );
runTest( 'odd5_NU_one' );
runTest( 'odd5_NU_inf' );
runTest( 'odd5_NU_frob' );
runTest( 'odd5_TL_max' );
runTest( 'odd5_TL_one' );
runTest( 'odd5_TL_inf' );
runTest( 'odd5_TL_frob' );
runTest( 'odd5_TU_max' );
runTest( 'odd5_TU_one' );
runTest( 'odd5_TU_inf' );
runTest( 'odd5_TU_frob' );

// All 8 dispatch paths x 4 norms for N=4 (even)
runTest( 'even4_NL_max' );
runTest( 'even4_NL_one' );
runTest( 'even4_NL_inf' );
runTest( 'even4_NL_frob' );
runTest( 'even4_NU_max' );
runTest( 'even4_NU_one' );
runTest( 'even4_NU_inf' );
runTest( 'even4_NU_frob' );
runTest( 'even4_TL_max' );
runTest( 'even4_TL_one' );
runTest( 'even4_TL_inf' );
runTest( 'even4_TL_frob' );
runTest( 'even4_TU_max' );
runTest( 'even4_TU_one' );
runTest( 'even4_TU_inf' );
runTest( 'even4_TU_frob' );

// Edge cases
runTest( 'n_zero_max' );
runTest( 'n_zero_one' );
runTest( 'n_zero_frob' );
runTest( 'n_one_max' );
runTest( 'n_one_one' );
runTest( 'n_one_frob' );

// Larger odd N=7 for extra coverage
runTest( 'odd7_NL_max' );
runTest( 'odd7_NL_one' );
runTest( 'odd7_NL_inf' );
runTest( 'odd7_NL_frob' );
runTest( 'odd7_NU_max' );
runTest( 'odd7_NU_one' );
runTest( 'odd7_NU_inf' );
runTest( 'odd7_NU_frob' );
runTest( 'odd7_TL_max' );
runTest( 'odd7_TL_one' );
runTest( 'odd7_TL_inf' );
runTest( 'odd7_TL_frob' );
runTest( 'odd7_TU_max' );
runTest( 'odd7_TU_one' );
runTest( 'odd7_TU_inf' );
runTest( 'odd7_TU_frob' );
