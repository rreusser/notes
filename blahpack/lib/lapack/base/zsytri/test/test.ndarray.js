/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zsytri = require( './../lib/base.js' );


// FIXTURES //

var fixtures = {
	'n1_upper': require( './fixtures/n1_upper.json' ),
	'n1_upper_factored': require( './fixtures/n1_upper_factored.json' ),
	'n1_lower': require( './fixtures/n1_lower.json' ),
	'n1_lower_factored': require( './fixtures/n1_lower_factored.json' ),
	'3x3_upper': require( './fixtures/3x3_upper.json' ),
	'3x3_upper_factored': require( './fixtures/3x3_upper_factored.json' ),
	'3x3_lower': require( './fixtures/3x3_lower.json' ),
	'3x3_lower_factored': require( './fixtures/3x3_lower_factored.json' ),
	'4x4_upper_indef': require( './fixtures/4x4_upper_indef.json' ),
	'4x4_upper_indef_factored': require( './fixtures/4x4_upper_indef_factored.json' ),
	'4x4_lower_indef': require( './fixtures/4x4_lower_indef.json' ),
	'4x4_lower_indef_factored': require( './fixtures/4x4_lower_indef_factored.json' ),
	'singular_lower': require( './fixtures/singular_lower.json' ),
	'singular_lower_factored': require( './fixtures/singular_lower_factored.json' ),
	'4x4_upper_swap': require( './fixtures/4x4_upper_swap.json' ),
	'4x4_upper_swap_factored': require( './fixtures/4x4_upper_swap_factored.json' ),
	'4x4_lower_swap': require( './fixtures/4x4_lower_swap.json' ),
	'4x4_lower_swap_factored': require( './fixtures/4x4_lower_swap_factored.json' ),
	'singular_upper': require( './fixtures/singular_upper.json' ),
	'singular_upper_factored': require( './fixtures/singular_upper_factored.json' )
};


// FUNCTIONS //

/**
* Convert Fortran 1-based IPIV to 0-based with bitwise-NOT convention for 2x2 pivots.
*
* @private
* @param {Array} ipivFortran - 1-based Fortran IPIV array
* @param {number} N - dimension
* @returns {Int32Array} 0-based IPIV
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
* Assert that two arrays are element-wise close.
*
* @private
* @param {Array} actual - actual values
* @param {Array} expected - expected values
* @param {number} tol - relative tolerance
* @param {string} msg - message prefix
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		if ( expected[ i ] === 0.0 ) {
			assert.ok( Math.abs( actual[ i ] ) <= tol, msg + '[' + i + ']: expected 0, got ' + actual[ i ] ); // eslint-disable-line max-len
		} else {
			relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
			assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
		}
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
* Run a zsytri fixture test case.
*
* @private
* @param {string} name - fixture case name (without _factored suffix)
* @param {string} uplo - 'upper' or 'lower'
*/
function runTest( name, uplo ) {
	var factored = fixtures[ name + '_factored' ];
	var expected = fixtures[ name ];
	var IPIV;
	var WORK;
	var info;
	var Av;
	var N;
	var A;

	N = factored.ipiv.length;

	// Build input complex array from factored data
	A = new Complex128Array( N * N );
	Av = reinterpret( A, 0 );
	Av.set( factored.a );

	// Convert IPIV from Fortran 1-based to JS 0-based
	IPIV = convertIPIV( factored.ipiv, N );

	// Allocate workspace
	WORK = new Complex128Array( N );

	// Call zsytri: column-major with strideA1=1, strideA2=N
	info = zsytri( uplo, N, A, 1, N, 0, IPIV, 1, 0, WORK, 1, 0 );

	assert.strictEqual( info, expected.info, name + ': info' );

	if ( expected.info === 0 ) {
		assertClose( toArray( Av ), expected.a, 1e-12, name + ': a' );
	}
}


// TESTS //

test( 'zsytri: main export is a function', function t() {
	assert.strictEqual( typeof zsytri, 'function' );
});

test( 'zsytri: n0', function t() {
	var info = zsytri( 'upper', 0, new Complex128Array( 0 ), 1, 0, 0, new Int32Array( 0 ), 1, 0, new Complex128Array( 0 ), 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
});

test( 'zsytri: n1_upper', function t() {
	runTest( 'n1_upper', 'upper' );
});

test( 'zsytri: n1_lower', function t() {
	runTest( 'n1_lower', 'lower' );
});

test( 'zsytri: 3x3_upper', function t() {
	runTest( '3x3_upper', 'upper' );
});

test( 'zsytri: 3x3_lower', function t() {
	runTest( '3x3_lower', 'lower' );
});

test( 'zsytri: 4x4_upper_indef', function t() {
	runTest( '4x4_upper_indef', 'upper' );
});

test( 'zsytri: 4x4_lower_indef', function t() {
	runTest( '4x4_lower_indef', 'lower' );
});

test( 'zsytri: singular_lower', function t() {
	runTest( 'singular_lower', 'lower' );
});

test( 'zsytri: 4x4_upper_swap', function t() {
	runTest( '4x4_upper_swap', 'upper' );
});

test( 'zsytri: 4x4_lower_swap', function t() {
	runTest( '4x4_lower_swap', 'lower' );
});

test( 'zsytri: singular_upper', function t() {
	runTest( 'singular_upper', 'upper' );
});
