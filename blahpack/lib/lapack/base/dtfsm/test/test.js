/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtfsm = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dtfsm.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {Array} actual - actual value
* @param {Array} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' ); // eslint-disable-line max-len
	}
}

/**
* Maps a single-character Fortran flag to a stdlib-js string.
*
* @private
* @param {string} key - parameter name
* @param {string} val - single-character flag
* @returns {string} stdlib-js string value
*/
function mapFlag( key, val ) {
	if ( key === 'side' ) {
		return ( val === 'L' ) ? 'left' : 'right';
	}
	if ( key === 'uplo' ) {
		return ( val === 'L' ) ? 'lower' : 'upper';
	}
	if ( key === 'diag' ) {
		return ( val === 'U' ) ? 'unit' : 'non-unit';
	}
	// transr, trans
	return ( val === 'N' ) ? 'no-transpose' : 'transpose';
}

/**
* Runs a fixture test case.
*
* @private
* @param {string} name - test case name
*/
function runCase( name ) {
	var expected;
	var transr;
	var actual;
	var alpha;
	var trans;
	var side;
	var uplo;
	var diag;
	var tc;
	var M;
	var N;
	var A;
	var B;
	var i;

	tc = findCase( name );
	transr = mapFlag( 'transr', tc.transr );
	side = mapFlag( 'side', tc.side );
	uplo = mapFlag( 'uplo', tc.uplo );
	trans = mapFlag( 'trans', tc.trans );
	diag = mapFlag( 'diag', tc.diag );
	M = tc.M;
	N = tc.N;
	alpha = tc.alpha;

	A = new Float64Array( tc.A );
	B = new Float64Array( tc.B_in.length );
	for ( i = 0; i < tc.B_in.length; i++ ) {
		B[ i ] = tc.B_in[ i ];
	}

	// Column-major: strideB1=1, strideB2=M
	dtfsm( transr, side, uplo, trans, diag, M, N, alpha, A, 1, 0, B, 1, M, 0 ); // eslint-disable-line max-len

	expected = tc.B_out;
	actual = [];
	for ( i = 0; i < expected.length; i++ ) {
		actual.push( B[ i ] );
	}
	assertArrayClose( actual, expected, 1e-14, name );
}


// TESTS //

test( 'dtfsm: left side, odd M, TRANSR=N, UPLO=L, TRANS=N', function t() {
	runCase( 'left_odd_NL_N' );
});

test( 'dtfsm: left side, odd M, TRANSR=N, UPLO=L, TRANS=T', function t() {
	runCase( 'left_odd_NL_T' );
});

test( 'dtfsm: left side, odd M, TRANSR=N, UPLO=U, TRANS=N', function t() {
	runCase( 'left_odd_NU_N' );
});

test( 'dtfsm: left side, odd M, TRANSR=N, UPLO=U, TRANS=T', function t() {
	runCase( 'left_odd_NU_T' );
});

test( 'dtfsm: left side, odd M, TRANSR=T, UPLO=L, TRANS=N', function t() {
	runCase( 'left_odd_TL_N' );
});

test( 'dtfsm: left side, odd M, TRANSR=T, UPLO=L, TRANS=T', function t() {
	runCase( 'left_odd_TL_T' );
});

test( 'dtfsm: left side, odd M, TRANSR=T, UPLO=U, TRANS=N', function t() {
	runCase( 'left_odd_TU_N' );
});

test( 'dtfsm: left side, odd M, TRANSR=T, UPLO=U, TRANS=T', function t() {
	runCase( 'left_odd_TU_T' );
});

test( 'dtfsm: left side, even M, TRANSR=N, UPLO=L, TRANS=N', function t() {
	runCase( 'left_even_NL_N' );
});

test( 'dtfsm: left side, even M, TRANSR=N, UPLO=L, TRANS=T', function t() {
	runCase( 'left_even_NL_T' );
});

test( 'dtfsm: left side, even M, TRANSR=N, UPLO=U, TRANS=N', function t() {
	runCase( 'left_even_NU_N' );
});

test( 'dtfsm: left side, even M, TRANSR=N, UPLO=U, TRANS=T', function t() {
	runCase( 'left_even_NU_T' );
});

test( 'dtfsm: left side, even M, TRANSR=T, UPLO=L, TRANS=N', function t() {
	runCase( 'left_even_TL_N' );
});

test( 'dtfsm: left side, even M, TRANSR=T, UPLO=L, TRANS=T', function t() {
	runCase( 'left_even_TL_T' );
});

test( 'dtfsm: left side, even M, TRANSR=T, UPLO=U, TRANS=N', function t() {
	runCase( 'left_even_TU_N' );
});

test( 'dtfsm: left side, even M, TRANSR=T, UPLO=U, TRANS=T', function t() {
	runCase( 'left_even_TU_T' );
});

test( 'dtfsm: right side, odd N, TRANSR=N, UPLO=L, TRANS=N', function t() {
	runCase( 'right_odd_NL_N' );
});

test( 'dtfsm: right side, odd N, TRANSR=N, UPLO=L, TRANS=T', function t() {
	runCase( 'right_odd_NL_T' );
});

test( 'dtfsm: right side, odd N, TRANSR=N, UPLO=U, TRANS=N', function t() {
	runCase( 'right_odd_NU_N' );
});

test( 'dtfsm: right side, odd N, TRANSR=N, UPLO=U, TRANS=T', function t() {
	runCase( 'right_odd_NU_T' );
});

test( 'dtfsm: right side, odd N, TRANSR=T, UPLO=L, TRANS=N', function t() {
	runCase( 'right_odd_TL_N' );
});

test( 'dtfsm: right side, odd N, TRANSR=T, UPLO=L, TRANS=T', function t() {
	runCase( 'right_odd_TL_T' );
});

test( 'dtfsm: right side, odd N, TRANSR=T, UPLO=U, TRANS=N', function t() {
	runCase( 'right_odd_TU_N' );
});

test( 'dtfsm: right side, odd N, TRANSR=T, UPLO=U, TRANS=T', function t() {
	runCase( 'right_odd_TU_T' );
});

test( 'dtfsm: right side, even N, TRANSR=N, UPLO=L, TRANS=N', function t() {
	runCase( 'right_even_NL_N' );
});

test( 'dtfsm: right side, even N, TRANSR=N, UPLO=L, TRANS=T', function t() {
	runCase( 'right_even_NL_T' );
});

test( 'dtfsm: right side, even N, TRANSR=N, UPLO=U, TRANS=N', function t() {
	runCase( 'right_even_NU_N' );
});

test( 'dtfsm: right side, even N, TRANSR=N, UPLO=U, TRANS=T', function t() {
	runCase( 'right_even_NU_T' );
});

test( 'dtfsm: right side, even N, TRANSR=T, UPLO=L, TRANS=N', function t() {
	runCase( 'right_even_TL_N' );
});

test( 'dtfsm: right side, even N, TRANSR=T, UPLO=L, TRANS=T', function t() {
	runCase( 'right_even_TL_T' );
});

test( 'dtfsm: right side, even N, TRANSR=T, UPLO=U, TRANS=N', function t() {
	runCase( 'right_even_TU_N' );
});

test( 'dtfsm: right side, even N, TRANSR=T, UPLO=U, TRANS=T', function t() {
	runCase( 'right_even_TU_T' );
});

test( 'dtfsm: unit diagonal (left, odd)', function t() {
	runCase( 'left_odd_NL_N_unit' );
});

test( 'dtfsm: unit diagonal (left, even)', function t() {
	runCase( 'left_even_NL_N_unit' );
});

test( 'dtfsm: unit diagonal (right, odd)', function t() {
	runCase( 'right_odd_NU_T_unit' );
});

test( 'dtfsm: unit diagonal (right, even)', function t() {
	runCase( 'right_even_TL_N_unit' );
});

test( 'dtfsm: alpha scaling (left, odd)', function t() {
	runCase( 'left_odd_alpha' );
});

test( 'dtfsm: alpha scaling (right, even)', function t() {
	runCase( 'right_even_alpha' );
});

test( 'dtfsm: M=0 early return', function t() {
	var A = new Float64Array( [ 1.0 ] );
	var B = new Float64Array( [ 99.0 ] );
	dtfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 0, 2, 1.0, A, 1, 0, B, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( B[ 0 ], 99.0 );
});

test( 'dtfsm: N=0 early return', function t() {
	var A = new Float64Array( [ 1.0 ] );
	var B = new Float64Array( [ 99.0 ] );
	dtfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 2, 0, 1.0, A, 1, 0, B, 1, 2, 0 ); // eslint-disable-line max-len
	assert.equal( B[ 0 ], 99.0 );
});

test( 'dtfsm: alpha=0 sets B to zero', function t() {
	var expected;
	var actual;
	var tc;
	var A;
	var B;
	var M;
	var N;
	var i;

	tc = findCase( 'alpha_zero' );
	M = tc.M;
	N = tc.N;
	A = new Float64Array( 6 );
	B = new Float64Array( tc.B_in.length );
	for ( i = 0; i < tc.B_in.length; i++ ) {
		B[ i ] = tc.B_in[ i ];
	}

	dtfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', M, N, 0.0, A, 1, 0, B, 1, M, 0 ); // eslint-disable-line max-len

	expected = tc.B_out;
	actual = [];
	for ( i = 0; i < expected.length; i++ ) {
		actual.push( B[ i ] );
	}
	assertArrayClose( actual, expected, 1e-14, 'alpha_zero' );
});

test( 'dtfsm: M=1 special case (left, odd, NL, N)', function t() {
	runCase( 'left_m1_NL_N' );
});

test( 'dtfsm: M=1 special case (left, odd, NL, T)', function t() {
	runCase( 'left_m1_NL_T' );
});

test( 'dtfsm: M=1 special case (left, trans, TL, N)', function t() {
	runCase( 'left_m1_TL_N' );
});

test( 'dtfsm: M=1 special case (left, trans, TL, T)', function t() {
	runCase( 'left_m1_TL_T' );
});

test( 'dtfsm: larger odd M=5', function t() {
	runCase( 'left_5_NL_N' );
});

test( 'dtfsm: larger odd M=5 (TU, T)', function t() {
	runCase( 'left_5_TU_T' );
});

test( 'dtfsm: larger even M=6', function t() {
	runCase( 'left_6_NU_N' );
});

test( 'dtfsm: larger even M=6 (TL, T)', function t() {
	runCase( 'left_6_TL_T' );
});
