/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-statements-per-line */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlanhf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zlanhf.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {Object} test case data
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
	var relErr;

	relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

/**
* Runs a single test case from fixtures.
*
* @private
* @param {string} caseName - fixture case name
* @param {string} norm - norm type for JS API
* @param {string} transr - transpose operation string
* @param {string} uplo - upper or lower triangle
* @param {integer} N - matrix order
* @param {string} rfpCase - fixture case containing rfp data
*/
function runCase( caseName, norm, transr, uplo, N, rfpCase ) {
	var result;
	var rfpTc;
	var work;
	var tc;
	var A;

	tc = findCase( caseName );
	rfpTc = findCase( rfpCase || caseName );
	A = new Complex128Array( rfpTc.rfp );
	work = new Float64Array( N );
	result = zlanhf( norm, transr, uplo, N, A, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, caseName );
}


// TESTS //

test( 'zlanhf: N=0 returns 0 for all norms', function t() {
	var result;
	var work;
	var A;

	A = new Complex128Array( 0 );
	work = new Float64Array( 0 );
	result = zlanhf( 'max', 'no-transpose', 'upper', 0, A, 1, 0, work, 1, 0 );
	assert.equal( result, 0.0 );
	result = zlanhf( 'one-norm', 'no-transpose', 'upper', 0, A, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
	result = zlanhf( 'frobenius', 'no-transpose', 'upper', 0, A, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});

test( 'zlanhf: N=1 returns abs(real(A(0)))', function t() {
	runCase( 'zlanhf_n1', 'max', 'no-transpose', 'upper', 1, 'zlanhf_n1' );
});

// N=5 (odd): TRANSR='N', UPLO='U'
test( 'zlanhf: N=5, transr=N, uplo=U, max', function t() {
	runCase( 'zlanhf_5_NU_max', 'max', 'no-transpose', 'upper', 5, 'zlanhf_5_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=N, uplo=U, one-norm', function t() {
	runCase( 'zlanhf_5_NU_one', 'one-norm', 'no-transpose', 'upper', 5, 'zlanhf_5_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=N, uplo=U, inf-norm', function t() {
	runCase( 'zlanhf_5_NU_inf', 'inf-norm', 'no-transpose', 'upper', 5, 'zlanhf_5_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=N, uplo=U, frobenius', function t() {
	runCase( 'zlanhf_5_NU_frob', 'frobenius', 'no-transpose', 'upper', 5, 'zlanhf_5_NU_max' ); // eslint-disable-line max-len
});

// N=5: TRANSR='C', UPLO='U'
test( 'zlanhf: N=5, transr=C, uplo=U, max', function t() {
	runCase( 'zlanhf_5_CU_max', 'max', 'conjugate-transpose', 'upper', 5, 'zlanhf_5_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=C, uplo=U, one-norm', function t() {
	runCase( 'zlanhf_5_CU_one', 'one-norm', 'conjugate-transpose', 'upper', 5, 'zlanhf_5_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=C, uplo=U, inf-norm', function t() {
	runCase( 'zlanhf_5_CU_inf', 'inf-norm', 'conjugate-transpose', 'upper', 5, 'zlanhf_5_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=C, uplo=U, frobenius', function t() {
	runCase( 'zlanhf_5_CU_frob', 'frobenius', 'conjugate-transpose', 'upper', 5, 'zlanhf_5_CU_max' ); // eslint-disable-line max-len
});

// N=5: TRANSR='N', UPLO='L'
test( 'zlanhf: N=5, transr=N, uplo=L, max', function t() {
	runCase( 'zlanhf_5_NL_max', 'max', 'no-transpose', 'lower', 5, 'zlanhf_5_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=N, uplo=L, one-norm', function t() {
	runCase( 'zlanhf_5_NL_one', 'one-norm', 'no-transpose', 'lower', 5, 'zlanhf_5_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=N, uplo=L, inf-norm', function t() {
	runCase( 'zlanhf_5_NL_inf', 'inf-norm', 'no-transpose', 'lower', 5, 'zlanhf_5_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=N, uplo=L, frobenius', function t() {
	runCase( 'zlanhf_5_NL_frob', 'frobenius', 'no-transpose', 'lower', 5, 'zlanhf_5_NL_max' ); // eslint-disable-line max-len
});

// N=5: TRANSR='C', UPLO='L'
test( 'zlanhf: N=5, transr=C, uplo=L, max', function t() {
	runCase( 'zlanhf_5_CL_max', 'max', 'conjugate-transpose', 'lower', 5, 'zlanhf_5_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=C, uplo=L, one-norm', function t() {
	runCase( 'zlanhf_5_CL_one', 'one-norm', 'conjugate-transpose', 'lower', 5, 'zlanhf_5_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=C, uplo=L, inf-norm', function t() {
	runCase( 'zlanhf_5_CL_inf', 'inf-norm', 'conjugate-transpose', 'lower', 5, 'zlanhf_5_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=5, transr=C, uplo=L, frobenius', function t() {
	runCase( 'zlanhf_5_CL_frob', 'frobenius', 'conjugate-transpose', 'lower', 5, 'zlanhf_5_CL_max' ); // eslint-disable-line max-len
});

// N=4 (even): TRANSR='N', UPLO='U'
test( 'zlanhf: N=4, transr=N, uplo=U, max', function t() {
	runCase( 'zlanhf_4_NU_max', 'max', 'no-transpose', 'upper', 4, 'zlanhf_4_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=N, uplo=U, one-norm', function t() {
	runCase( 'zlanhf_4_NU_one', 'one-norm', 'no-transpose', 'upper', 4, 'zlanhf_4_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=N, uplo=U, inf-norm', function t() {
	runCase( 'zlanhf_4_NU_inf', 'inf-norm', 'no-transpose', 'upper', 4, 'zlanhf_4_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=N, uplo=U, frobenius', function t() {
	runCase( 'zlanhf_4_NU_frob', 'frobenius', 'no-transpose', 'upper', 4, 'zlanhf_4_NU_max' ); // eslint-disable-line max-len
});

// N=4: TRANSR='C', UPLO='U'
test( 'zlanhf: N=4, transr=C, uplo=U, max', function t() {
	runCase( 'zlanhf_4_CU_max', 'max', 'conjugate-transpose', 'upper', 4, 'zlanhf_4_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=C, uplo=U, one-norm', function t() {
	runCase( 'zlanhf_4_CU_one', 'one-norm', 'conjugate-transpose', 'upper', 4, 'zlanhf_4_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=C, uplo=U, inf-norm', function t() {
	runCase( 'zlanhf_4_CU_inf', 'inf-norm', 'conjugate-transpose', 'upper', 4, 'zlanhf_4_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=C, uplo=U, frobenius', function t() {
	runCase( 'zlanhf_4_CU_frob', 'frobenius', 'conjugate-transpose', 'upper', 4, 'zlanhf_4_CU_max' ); // eslint-disable-line max-len
});

// N=4: TRANSR='N', UPLO='L'
test( 'zlanhf: N=4, transr=N, uplo=L, max', function t() {
	runCase( 'zlanhf_4_NL_max', 'max', 'no-transpose', 'lower', 4, 'zlanhf_4_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=N, uplo=L, one-norm', function t() {
	runCase( 'zlanhf_4_NL_one', 'one-norm', 'no-transpose', 'lower', 4, 'zlanhf_4_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=N, uplo=L, inf-norm', function t() {
	runCase( 'zlanhf_4_NL_inf', 'inf-norm', 'no-transpose', 'lower', 4, 'zlanhf_4_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=N, uplo=L, frobenius', function t() {
	runCase( 'zlanhf_4_NL_frob', 'frobenius', 'no-transpose', 'lower', 4, 'zlanhf_4_NL_max' ); // eslint-disable-line max-len
});

// N=4: TRANSR='C', UPLO='L'
test( 'zlanhf: N=4, transr=C, uplo=L, max', function t() {
	runCase( 'zlanhf_4_CL_max', 'max', 'conjugate-transpose', 'lower', 4, 'zlanhf_4_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=C, uplo=L, one-norm', function t() {
	runCase( 'zlanhf_4_CL_one', 'one-norm', 'conjugate-transpose', 'lower', 4, 'zlanhf_4_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=C, uplo=L, inf-norm', function t() {
	runCase( 'zlanhf_4_CL_inf', 'inf-norm', 'conjugate-transpose', 'lower', 4, 'zlanhf_4_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=4, transr=C, uplo=L, frobenius', function t() {
	runCase( 'zlanhf_4_CL_frob', 'frobenius', 'conjugate-transpose', 'lower', 4, 'zlanhf_4_CL_max' ); // eslint-disable-line max-len
});

// N=3 (odd): TRANSR='N', UPLO='U'
test( 'zlanhf: N=3, transr=N, uplo=U, max', function t() {
	runCase( 'zlanhf_3_NU_max', 'max', 'no-transpose', 'upper', 3, 'zlanhf_3_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=N, uplo=U, one-norm', function t() {
	runCase( 'zlanhf_3_NU_one', 'one-norm', 'no-transpose', 'upper', 3, 'zlanhf_3_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=N, uplo=U, inf-norm', function t() {
	runCase( 'zlanhf_3_NU_inf', 'inf-norm', 'no-transpose', 'upper', 3, 'zlanhf_3_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=N, uplo=U, frobenius', function t() {
	runCase( 'zlanhf_3_NU_frob', 'frobenius', 'no-transpose', 'upper', 3, 'zlanhf_3_NU_max' ); // eslint-disable-line max-len
});

// N=3: TRANSR='C', UPLO='U'
test( 'zlanhf: N=3, transr=C, uplo=U, max', function t() {
	runCase( 'zlanhf_3_CU_max', 'max', 'conjugate-transpose', 'upper', 3, 'zlanhf_3_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=C, uplo=U, one-norm', function t() {
	runCase( 'zlanhf_3_CU_one', 'one-norm', 'conjugate-transpose', 'upper', 3, 'zlanhf_3_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=C, uplo=U, inf-norm', function t() {
	runCase( 'zlanhf_3_CU_inf', 'inf-norm', 'conjugate-transpose', 'upper', 3, 'zlanhf_3_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=C, uplo=U, frobenius', function t() {
	runCase( 'zlanhf_3_CU_frob', 'frobenius', 'conjugate-transpose', 'upper', 3, 'zlanhf_3_CU_max' ); // eslint-disable-line max-len
});

// N=3: TRANSR='N', UPLO='L'
test( 'zlanhf: N=3, transr=N, uplo=L, max', function t() {
	runCase( 'zlanhf_3_NL_max', 'max', 'no-transpose', 'lower', 3, 'zlanhf_3_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=N, uplo=L, one-norm', function t() {
	runCase( 'zlanhf_3_NL_one', 'one-norm', 'no-transpose', 'lower', 3, 'zlanhf_3_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=N, uplo=L, inf-norm', function t() {
	runCase( 'zlanhf_3_NL_inf', 'inf-norm', 'no-transpose', 'lower', 3, 'zlanhf_3_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=N, uplo=L, frobenius', function t() {
	runCase( 'zlanhf_3_NL_frob', 'frobenius', 'no-transpose', 'lower', 3, 'zlanhf_3_NL_max' ); // eslint-disable-line max-len
});

// N=3: TRANSR='C', UPLO='L'
test( 'zlanhf: N=3, transr=C, uplo=L, max', function t() {
	runCase( 'zlanhf_3_CL_max', 'max', 'conjugate-transpose', 'lower', 3, 'zlanhf_3_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=C, uplo=L, one-norm', function t() {
	runCase( 'zlanhf_3_CL_one', 'one-norm', 'conjugate-transpose', 'lower', 3, 'zlanhf_3_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=C, uplo=L, inf-norm', function t() {
	runCase( 'zlanhf_3_CL_inf', 'inf-norm', 'conjugate-transpose', 'lower', 3, 'zlanhf_3_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=3, transr=C, uplo=L, frobenius', function t() {
	runCase( 'zlanhf_3_CL_frob', 'frobenius', 'conjugate-transpose', 'lower', 3, 'zlanhf_3_CL_max' ); // eslint-disable-line max-len
});

// N=2 (even): TRANSR='N', UPLO='U'
test( 'zlanhf: N=2, transr=N, uplo=U, max', function t() {
	runCase( 'zlanhf_2_NU_max', 'max', 'no-transpose', 'upper', 2, 'zlanhf_2_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=2, transr=N, uplo=U, one-norm', function t() {
	runCase( 'zlanhf_2_NU_one', 'one-norm', 'no-transpose', 'upper', 2, 'zlanhf_2_NU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=2, transr=N, uplo=U, frobenius', function t() {
	runCase( 'zlanhf_2_NU_frob', 'frobenius', 'no-transpose', 'upper', 2, 'zlanhf_2_NU_max' ); // eslint-disable-line max-len
});

// N=2: TRANSR='C', UPLO='U'
test( 'zlanhf: N=2, transr=C, uplo=U, max', function t() {
	runCase( 'zlanhf_2_CU_max', 'max', 'conjugate-transpose', 'upper', 2, 'zlanhf_2_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=2, transr=C, uplo=U, one-norm', function t() {
	runCase( 'zlanhf_2_CU_one', 'one-norm', 'conjugate-transpose', 'upper', 2, 'zlanhf_2_CU_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=2, transr=C, uplo=U, frobenius', function t() {
	runCase( 'zlanhf_2_CU_frob', 'frobenius', 'conjugate-transpose', 'upper', 2, 'zlanhf_2_CU_max' ); // eslint-disable-line max-len
});

// N=2: TRANSR='N', UPLO='L'
test( 'zlanhf: N=2, transr=N, uplo=L, max', function t() {
	runCase( 'zlanhf_2_NL_max', 'max', 'no-transpose', 'lower', 2, 'zlanhf_2_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=2, transr=N, uplo=L, one-norm', function t() {
	runCase( 'zlanhf_2_NL_one', 'one-norm', 'no-transpose', 'lower', 2, 'zlanhf_2_NL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=2, transr=N, uplo=L, frobenius', function t() {
	runCase( 'zlanhf_2_NL_frob', 'frobenius', 'no-transpose', 'lower', 2, 'zlanhf_2_NL_max' ); // eslint-disable-line max-len
});

// N=2: TRANSR='C', UPLO='L'
test( 'zlanhf: N=2, transr=C, uplo=L, max', function t() {
	runCase( 'zlanhf_2_CL_max', 'max', 'conjugate-transpose', 'lower', 2, 'zlanhf_2_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=2, transr=C, uplo=L, one-norm', function t() {
	runCase( 'zlanhf_2_CL_one', 'one-norm', 'conjugate-transpose', 'lower', 2, 'zlanhf_2_CL_max' ); // eslint-disable-line max-len
});
test( 'zlanhf: N=2, transr=C, uplo=L, frobenius', function t() {
	runCase( 'zlanhf_2_CL_frob', 'frobenius', 'conjugate-transpose', 'lower', 2, 'zlanhf_2_CL_max' ); // eslint-disable-line max-len
});
