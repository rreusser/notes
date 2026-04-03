/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, no-mixed-operators, node/no-sync */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zspcon = require( '../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zspcon.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


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
	});
}

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
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Converts Fortran 1-based IPIV to 0-based with bitwise-NOT convention for 2x2 pivots.
*
* @private
* @param {Array} ipivFortran - Fortran 1-based pivot indices
* @param {integer} N - number of pivots
* @returns {Int32Array} 0-based pivot indices
*/
function convertIPIV( ipivFortran, N ) {
	var out = new Int32Array( N );
	var i;
	for ( i = 0; i < N; i++ ) {
		if ( ipivFortran[ i ] > 0 ) {
			out[ i ] = ipivFortran[ i ] - 1;
		} else {
			out[ i ] = ~( -ipivFortran[ i ] - 1 );
		}
	}
	return out;
}

/**
* Builds a Complex128Array from a flat array of interleaved doubles.
*
* @private
* @param {Array} flatDoubles - flat array of interleaved re/im pairs
* @param {integer} nComplex - number of complex elements
* @returns {Complex128Array} complex array
*/
function buildComplex( flatDoubles, nComplex ) {
	var out = new Complex128Array( nComplex );
	var ov = reinterpret( out, 0 );
	var i;
	for ( i = 0; i < 2 * nComplex; i++ ) {
		ov[ i ] = flatDoubles[ i ];
	}
	return out;
}

/**
* Runs a fixture-based test: loads factored AP and IPIV, runs zspcon, and verifies rcond against fixture.
*
* @private
* @param {string} name - fixture test case name
* @param {string} uplo - `'upper'` or `'lower'`
* @param {number} N - matrix order
*/
function runFixtureTest( name, uplo, N ) {
	var rcond;
	var WORK;
	var IPIV;
	var info;
	var AP;
	var nn;
	var tc;

	tc = findCase( name );
	nn = N * ( N + 1 ) / 2;

	// Build factored AP (packed, nn complex elements)
	AP = buildComplex( tc.AP_factored, nn );
	IPIV = convertIPIV( tc.ipiv, N );

	WORK = new Complex128Array( 2 * N );
	rcond = new Float64Array( 1 );

	info = zspcon( uplo, N, AP, 1, 0, IPIV, 1, 0, tc.anorm, rcond, WORK, 1, 0 );
	assert.strictEqual( info, tc.info, name + ': info' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-10, name + ': rcond' );
}


// TESTS //

test( 'zspcon is a function', function t() {
	assert.strictEqual( typeof zspcon, 'function' );
});

test( 'zspcon: N=0 returns rcond=1', function t() {
	var rcond;
	var WORK;
	var IPIV;
	var info;
	var AP;
	var tc;

	tc = findCase( 'n_zero' );
	AP = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	WORK = new Complex128Array( 1 );
	rcond = new Float64Array( 1 );

	info = zspcon( 'upper', 0, AP, 1, 0, IPIV, 1, 0, 0.0, rcond, WORK, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rcond[ 0 ], tc.rcond );
});

test( 'zspcon: upper, 3x3, well-conditioned', function t() {
	runFixtureTest( 'upper_well_cond', 'upper', 3 );
});

test( 'zspcon: lower, 3x3, well-conditioned', function t() {
	runFixtureTest( 'lower_well_cond', 'lower', 3 );
});

test( 'zspcon: N=1 (upper)', function t() {
	runFixtureTest( 'n_one_upper', 'upper', 1 );
});

test( 'zspcon: N=1 (lower)', function t() {
	runFixtureTest( 'n_one_lower', 'lower', 1 );
});

test( 'zspcon: identity 3x3 (upper)', function t() {
	runFixtureTest( 'identity_upper', 'upper', 3 );
});

test( 'zspcon: identity 3x3 (lower)', function t() {
	runFixtureTest( 'identity_lower', 'lower', 3 );
});

test( 'zspcon: ill-conditioned diagonal (upper)', function t() {
	runFixtureTest( 'ill_cond_upper', 'upper', 3 );
});

test( 'zspcon: singular 3x3 (upper, rcond=0)', function t() {
	runFixtureTest( 'singular_upper', 'upper', 3 );
});

test( 'zspcon: 4x4 (upper)', function t() {
	runFixtureTest( '4x4_upper', 'upper', 4 );
});

test( 'zspcon: 4x4 (lower)', function t() {
	runFixtureTest( '4x4_lower', 'lower', 4 );
});

test( 'zspcon: purely imaginary diagonal (upper)', function t() {
	runFixtureTest( 'imag_diag_upper', 'upper', 3 );
});

test( 'zspcon: anorm=0 returns rcond=0', function t() {
	var rcond;
	var WORK;
	var IPIV;
	var info;
	var APv;
	var AP;

	// 2x2 packed identity (upper): A(1,1)=1, A(1,2)=0, A(2,2)=1
	AP = new Complex128Array( 3 );
	APv = reinterpret( AP, 0 );
	APv[ 0 ] = 1.0;
	APv[ 1 ] = 0.0;
	APv[ 2 ] = 0.0;
	APv[ 3 ] = 0.0;
	APv[ 4 ] = 1.0;
	APv[ 5 ] = 0.0;
	IPIV = new Int32Array([ 0, 1 ]);
	WORK = new Complex128Array( 4 );
	rcond = new Float64Array( 1 );

	info = zspcon( 'upper', 2, AP, 1, 0, IPIV, 1, 0, 0.0, rcond, WORK, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 0.0 );
});
