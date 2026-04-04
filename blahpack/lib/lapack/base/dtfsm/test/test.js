/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtfsm = require( './../lib/base.js' );


// FIXTURES //

var alphaZero = require( './fixtures/alpha_zero.json' );
var leftOddNLN = require( './fixtures/left_odd_nl_n.json' );
var leftOddNLT = require( './fixtures/left_odd_nl_t.json' );
var leftOddNUN = require( './fixtures/left_odd_nu_n.json' );
var leftOddNUT = require( './fixtures/left_odd_nu_t.json' );
var leftOddTLN = require( './fixtures/left_odd_tl_n.json' );
var leftOddTLT = require( './fixtures/left_odd_tl_t.json' );
var leftOddTUN = require( './fixtures/left_odd_tu_n.json' );
var leftOddTUT = require( './fixtures/left_odd_tu_t.json' );
var leftOddNLNUnit = require( './fixtures/left_odd_nl_n_unit.json' );
var leftOddAlpha = require( './fixtures/left_odd_alpha.json' );
var leftEvenNLN = require( './fixtures/left_even_nl_n.json' );
var leftEvenNLT = require( './fixtures/left_even_nl_t.json' );
var leftEvenNUN = require( './fixtures/left_even_nu_n.json' );
var leftEvenNUT = require( './fixtures/left_even_nu_t.json' );
var leftEvenTLN = require( './fixtures/left_even_tl_n.json' );
var leftEvenTLT = require( './fixtures/left_even_tl_t.json' );
var leftEvenTUN = require( './fixtures/left_even_tu_n.json' );
var leftEvenTUT = require( './fixtures/left_even_tu_t.json' );
var leftEvenNLNUnit = require( './fixtures/left_even_nl_n_unit.json' );
var leftM1NLN = require( './fixtures/left_m1_nl_n.json' );
var leftM1NLT = require( './fixtures/left_m1_nl_t.json' );
var leftM1TLN = require( './fixtures/left_m1_tl_n.json' );
var leftM1TLT = require( './fixtures/left_m1_tl_t.json' );
var left5NLN = require( './fixtures/left_5_nl_n.json' );
var left5TUT = require( './fixtures/left_5_tu_t.json' );
var left6NUN = require( './fixtures/left_6_nu_n.json' );
var left6TLT = require( './fixtures/left_6_tl_t.json' );
var rightOddNLN = require( './fixtures/right_odd_nl_n.json' );
var rightOddNLT = require( './fixtures/right_odd_nl_t.json' );
var rightOddNUN = require( './fixtures/right_odd_nu_n.json' );
var rightOddNUT = require( './fixtures/right_odd_nu_t.json' );
var rightOddTLN = require( './fixtures/right_odd_tl_n.json' );
var rightOddTLT = require( './fixtures/right_odd_tl_t.json' );
var rightOddTUN = require( './fixtures/right_odd_tu_n.json' );
var rightOddTUT = require( './fixtures/right_odd_tu_t.json' );
var rightOddNUTUnit = require( './fixtures/right_odd_nu_t_unit.json' );
var rightEvenNLN = require( './fixtures/right_even_nl_n.json' );
var rightEvenNLT = require( './fixtures/right_even_nl_t.json' );
var rightEvenNUN = require( './fixtures/right_even_nu_n.json' );
var rightEvenNUT = require( './fixtures/right_even_nu_t.json' );
var rightEvenTLN = require( './fixtures/right_even_tl_n.json' );
var rightEvenTLT = require( './fixtures/right_even_tl_t.json' );
var rightEvenTUN = require( './fixtures/right_even_tu_n.json' );
var rightEvenTUT = require( './fixtures/right_even_tu_t.json' );
var rightEvenTLNUnit = require( './fixtures/right_even_tl_n_unit.json' );
var rightEvenAlpha = require( './fixtures/right_even_alpha.json' );

var fixtures = {
	'left_odd_NL_N': leftOddNLN,
	'left_odd_NL_T': leftOddNLT,
	'left_odd_NU_N': leftOddNUN,
	'left_odd_NU_T': leftOddNUT,
	'left_odd_TL_N': leftOddTLN,
	'left_odd_TL_T': leftOddTLT,
	'left_odd_TU_N': leftOddTUN,
	'left_odd_TU_T': leftOddTUT,
	'left_odd_NL_N_unit': leftOddNLNUnit,
	'left_odd_alpha': leftOddAlpha,
	'left_even_NL_N': leftEvenNLN,
	'left_even_NL_T': leftEvenNLT,
	'left_even_NU_N': leftEvenNUN,
	'left_even_NU_T': leftEvenNUT,
	'left_even_TL_N': leftEvenTLN,
	'left_even_TL_T': leftEvenTLT,
	'left_even_TU_N': leftEvenTUN,
	'left_even_TU_T': leftEvenTUT,
	'left_even_NL_N_unit': leftEvenNLNUnit,
	'left_m1_NL_N': leftM1NLN,
	'left_m1_NL_T': leftM1NLT,
	'left_m1_TL_N': leftM1TLN,
	'left_m1_TL_T': leftM1TLT,
	'left_5_NL_N': left5NLN,
	'left_5_TU_T': left5TUT,
	'left_6_NU_N': left6NUN,
	'left_6_TL_T': left6TLT,
	'right_odd_NL_N': rightOddNLN,
	'right_odd_NL_T': rightOddNLT,
	'right_odd_NU_N': rightOddNUN,
	'right_odd_NU_T': rightOddNUT,
	'right_odd_TL_N': rightOddTLN,
	'right_odd_TL_T': rightOddTLT,
	'right_odd_TU_N': rightOddTUN,
	'right_odd_TU_T': rightOddTUT,
	'right_odd_NU_T_unit': rightOddNUTUnit,
	'right_even_NL_N': rightEvenNLN,
	'right_even_NL_T': rightEvenNLT,
	'right_even_NU_N': rightEvenNUN,
	'right_even_NU_T': rightEvenNUT,
	'right_even_TL_N': rightEvenTLN,
	'right_even_TL_T': rightEvenTLT,
	'right_even_TU_N': rightEvenTUN,
	'right_even_TU_T': rightEvenTUT,
	'right_even_TL_N_unit': rightEvenTLNUnit,
	'right_even_alpha': rightEvenAlpha,
	'alpha_zero': alphaZero
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

	tc = fixtures[ name ];
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

	tc = alphaZero;
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
