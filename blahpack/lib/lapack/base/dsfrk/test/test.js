/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, node/no-sync, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsfrk = require( './../lib' );
var base = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsfrk.jsonl' ), 'utf8' ).trim().split( '\n' );
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
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts two arrays are element-wise approximately equal.
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
* Decode fixture name to get dsfrk parameters.
*
* @private
* @param {string} name - fixture name like 'odd_NLN'
* @returns {Object} object with transr, uplo, trans strings
*/
function decodeParams( name ) {
	var transr;
	var parts;
	var trans;
	var uplo;
	var code;

	parts = name.split( '_' );
	code = parts[ parts.length - 1 ];
	transr = ( code[ 0 ] === 'N' ) ? 'no-transpose' : 'transpose';
	uplo = ( code[ 1 ] === 'L' ) ? 'lower' : 'upper';
	trans = ( code[ 2 ] === 'N' ) ? 'no-transpose' : 'transpose';
	return {
		'transr': transr,
		'uplo': uplo,
		'trans': trans
	};
}

/**
* Run a standard dsfrk fixture test.
*
* @private
* @param {string} name - fixture name
*/
function runCase( name ) {
	var params;
	var nrowa;
	var tc;
	var cA;
	var cC;
	var N;
	var K;

	tc = findCase( name );
	params = decodeParams( name );
	N = tc.n;
	K = tc.k;

	if ( params.trans === 'no-transpose' ) {
		nrowa = N;
	} else {
		nrowa = K;
	}

	cA = new Float64Array( tc.A );
	cC = new Float64Array( tc.c_in );

	base( params.transr, params.uplo, params.trans, N, K, tc.alpha, cA, 1, nrowa, 0, tc.beta, cC, 1, 0 );

	assertArrayClose( cC, tc.c_out, 1e-14, name );
}


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dsfrk, 'function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dsfrk.ndarray, 'function' );
});

// N=3 (odd), all 8 transr/uplo/trans combinations

test( 'dsfrk: odd, N, L, N', function t() {
	runCase( 'odd_NLN' );
});

test( 'dsfrk: odd, N, L, T', function t() {
	runCase( 'odd_NLT' );
});

test( 'dsfrk: odd, N, U, N', function t() {
	runCase( 'odd_NUN' );
});

test( 'dsfrk: odd, N, U, T', function t() {
	runCase( 'odd_NUT' );
});

test( 'dsfrk: odd, T, L, N', function t() {
	runCase( 'odd_TLN' );
});

test( 'dsfrk: odd, T, L, T', function t() {
	runCase( 'odd_TLT' );
});

test( 'dsfrk: odd, T, U, N', function t() {
	runCase( 'odd_TUN' );
});

test( 'dsfrk: odd, T, U, T', function t() {
	runCase( 'odd_TUT' );
});

// N=4 (even), all 8 combinations

test( 'dsfrk: even, N, L, N', function t() {
	runCase( 'even_NLN' );
});

test( 'dsfrk: even, N, L, T', function t() {
	runCase( 'even_NLT' );
});

test( 'dsfrk: even, N, U, N', function t() {
	runCase( 'even_NUN' );
});

test( 'dsfrk: even, N, U, T', function t() {
	runCase( 'even_NUT' );
});

test( 'dsfrk: even, T, L, N', function t() {
	runCase( 'even_TLN' );
});

test( 'dsfrk: even, T, L, T', function t() {
	runCase( 'even_TLT' );
});

test( 'dsfrk: even, T, U, N', function t() {
	runCase( 'even_TUN' );
});

test( 'dsfrk: even, T, U, T', function t() {
	runCase( 'even_TUT' );
});

// Larger N=5 (odd)

test( 'dsfrk: N=5, N, L, N', function t() {
	runCase( 'odd5_NLN' );
});

test( 'dsfrk: N=5, T, U, T', function t() {
	runCase( 'odd5_TUT' );
});

// Edge cases

test( 'dsfrk: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var cC = new Float64Array( tc.c_in );
	var cA = new Float64Array( [ 1.0 ] );

	base( 'no-transpose', 'lower', 'no-transpose', 0, 1, 1.0, cA, 1, 1, 0, 0.0, cC, 1, 0 );

	assertArrayClose( cC, tc.c_out, 1e-14, 'n_zero' );
});

test( 'dsfrk: alpha=0, beta=1 quick return', function t() {
	var tc = findCase( 'alpha_zero_beta_one' );
	var cC = new Float64Array( tc.c_in );
	var cA = new Float64Array( 6 );

	base( 'no-transpose', 'lower', 'no-transpose', 3, 2, 0.0, cA, 1, 3, 0, 1.0, cC, 1, 0 );

	assertArrayClose( cC, tc.c_out, 1e-14, 'alpha_zero_beta_one' );
});

test( 'dsfrk: alpha=0, beta=0 zeroes C', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var cC = new Float64Array( tc.c_in );
	var cA = new Float64Array( 6 );

	base( 'no-transpose', 'lower', 'no-transpose', 3, 2, 0.0, cA, 1, 3, 0, 0.0, cC, 1, 0 );

	assertArrayClose( cC, tc.c_out, 1e-14, 'alpha_zero_beta_zero' );
});

test( 'dsfrk: K=0, beta=1 quick return', function t() {
	var tc = findCase( 'k_zero_beta_one' );
	var cC = new Float64Array( tc.c_in );
	var cA = new Float64Array( 1 );

	base( 'no-transpose', 'lower', 'no-transpose', 3, 0, 1.0, cA, 1, 3, 0, 1.0, cC, 1, 0 );

	assertArrayClose( cC, tc.c_out, 1e-14, 'k_zero_beta_one' );
});

// Validation

test( 'ndarray: throws on invalid transr', function t() {
	var cC = new Float64Array( 6 );
	var cA = new Float64Array( 6 );

	assert.throws( function throws() {
		dsfrk.ndarray( 'invalid', 'lower', 'no-transpose', 3, 2, 1.0, cA, 1, 3, 0, 1.0, cC, 1, 0 );
	}, /TypeError/ );
});

test( 'ndarray: throws on invalid uplo', function t() {
	var cC = new Float64Array( 6 );
	var cA = new Float64Array( 6 );

	assert.throws( function throws() {
		dsfrk.ndarray( 'no-transpose', 'invalid', 'no-transpose', 3, 2, 1.0, cA, 1, 3, 0, 1.0, cC, 1, 0 );
	}, /TypeError/ );
});

test( 'ndarray: throws on invalid trans', function t() {
	var cC = new Float64Array( 6 );
	var cA = new Float64Array( 6 );

	assert.throws( function throws() {
		dsfrk.ndarray( 'no-transpose', 'lower', 'invalid', 3, 2, 1.0, cA, 1, 3, 0, 1.0, cC, 1, 0 );
	}, /TypeError/ );
});
