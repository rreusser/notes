/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-statements-per-line */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlanhf = require( './../lib/base.js' );


// FIXTURES //

var zlanhf_2_cl_frob = require( './fixtures/zlanhf_2_cl_frob.json' );
var zlanhf_2_cl_max = require( './fixtures/zlanhf_2_cl_max.json' );
var zlanhf_2_cl_one = require( './fixtures/zlanhf_2_cl_one.json' );
var zlanhf_2_cu_frob = require( './fixtures/zlanhf_2_cu_frob.json' );
var zlanhf_2_cu_max = require( './fixtures/zlanhf_2_cu_max.json' );
var zlanhf_2_cu_one = require( './fixtures/zlanhf_2_cu_one.json' );
var zlanhf_2_nl_frob = require( './fixtures/zlanhf_2_nl_frob.json' );
var zlanhf_2_nl_max = require( './fixtures/zlanhf_2_nl_max.json' );
var zlanhf_2_nl_one = require( './fixtures/zlanhf_2_nl_one.json' );
var zlanhf_2_nu_frob = require( './fixtures/zlanhf_2_nu_frob.json' );
var zlanhf_2_nu_max = require( './fixtures/zlanhf_2_nu_max.json' );
var zlanhf_2_nu_one = require( './fixtures/zlanhf_2_nu_one.json' );
var zlanhf_3_cl_frob = require( './fixtures/zlanhf_3_cl_frob.json' );
var zlanhf_3_cl_inf = require( './fixtures/zlanhf_3_cl_inf.json' );
var zlanhf_3_cl_max = require( './fixtures/zlanhf_3_cl_max.json' );
var zlanhf_3_cl_one = require( './fixtures/zlanhf_3_cl_one.json' );
var zlanhf_3_cu_frob = require( './fixtures/zlanhf_3_cu_frob.json' );
var zlanhf_3_cu_inf = require( './fixtures/zlanhf_3_cu_inf.json' );
var zlanhf_3_cu_max = require( './fixtures/zlanhf_3_cu_max.json' );
var zlanhf_3_cu_one = require( './fixtures/zlanhf_3_cu_one.json' );
var zlanhf_3_nl_frob = require( './fixtures/zlanhf_3_nl_frob.json' );
var zlanhf_3_nl_inf = require( './fixtures/zlanhf_3_nl_inf.json' );
var zlanhf_3_nl_max = require( './fixtures/zlanhf_3_nl_max.json' );
var zlanhf_3_nl_one = require( './fixtures/zlanhf_3_nl_one.json' );
var zlanhf_3_nu_frob = require( './fixtures/zlanhf_3_nu_frob.json' );
var zlanhf_3_nu_inf = require( './fixtures/zlanhf_3_nu_inf.json' );
var zlanhf_3_nu_max = require( './fixtures/zlanhf_3_nu_max.json' );
var zlanhf_3_nu_one = require( './fixtures/zlanhf_3_nu_one.json' );
var zlanhf_4_cl_frob = require( './fixtures/zlanhf_4_cl_frob.json' );
var zlanhf_4_cl_inf = require( './fixtures/zlanhf_4_cl_inf.json' );
var zlanhf_4_cl_max = require( './fixtures/zlanhf_4_cl_max.json' );
var zlanhf_4_cl_one = require( './fixtures/zlanhf_4_cl_one.json' );
var zlanhf_4_cu_frob = require( './fixtures/zlanhf_4_cu_frob.json' );
var zlanhf_4_cu_inf = require( './fixtures/zlanhf_4_cu_inf.json' );
var zlanhf_4_cu_max = require( './fixtures/zlanhf_4_cu_max.json' );
var zlanhf_4_cu_one = require( './fixtures/zlanhf_4_cu_one.json' );
var zlanhf_4_nl_frob = require( './fixtures/zlanhf_4_nl_frob.json' );
var zlanhf_4_nl_inf = require( './fixtures/zlanhf_4_nl_inf.json' );
var zlanhf_4_nl_max = require( './fixtures/zlanhf_4_nl_max.json' );
var zlanhf_4_nl_one = require( './fixtures/zlanhf_4_nl_one.json' );
var zlanhf_4_nu_frob = require( './fixtures/zlanhf_4_nu_frob.json' );
var zlanhf_4_nu_inf = require( './fixtures/zlanhf_4_nu_inf.json' );
var zlanhf_4_nu_max = require( './fixtures/zlanhf_4_nu_max.json' );
var zlanhf_4_nu_one = require( './fixtures/zlanhf_4_nu_one.json' );
var zlanhf_5_cl_frob = require( './fixtures/zlanhf_5_cl_frob.json' );
var zlanhf_5_cl_inf = require( './fixtures/zlanhf_5_cl_inf.json' );
var zlanhf_5_cl_max = require( './fixtures/zlanhf_5_cl_max.json' );
var zlanhf_5_cl_one = require( './fixtures/zlanhf_5_cl_one.json' );
var zlanhf_5_cu_frob = require( './fixtures/zlanhf_5_cu_frob.json' );
var zlanhf_5_cu_inf = require( './fixtures/zlanhf_5_cu_inf.json' );
var zlanhf_5_cu_max = require( './fixtures/zlanhf_5_cu_max.json' );
var zlanhf_5_cu_one = require( './fixtures/zlanhf_5_cu_one.json' );
var zlanhf_5_nl_frob = require( './fixtures/zlanhf_5_nl_frob.json' );
var zlanhf_5_nl_inf = require( './fixtures/zlanhf_5_nl_inf.json' );
var zlanhf_5_nl_max = require( './fixtures/zlanhf_5_nl_max.json' );
var zlanhf_5_nl_one = require( './fixtures/zlanhf_5_nl_one.json' );
var zlanhf_5_nu_frob = require( './fixtures/zlanhf_5_nu_frob.json' );
var zlanhf_5_nu_inf = require( './fixtures/zlanhf_5_nu_inf.json' );
var zlanhf_5_nu_max = require( './fixtures/zlanhf_5_nu_max.json' );
var zlanhf_5_nu_one = require( './fixtures/zlanhf_5_nu_one.json' );
var zlanhf_n0 = require( './fixtures/zlanhf_n0.json' );
var zlanhf_n1 = require( './fixtures/zlanhf_n1.json' );

var fixtures = {
	'zlanhf_2_CL_frob': zlanhf_2_cl_frob,
	'zlanhf_2_CL_max': zlanhf_2_cl_max,
	'zlanhf_2_CL_one': zlanhf_2_cl_one,
	'zlanhf_2_CU_frob': zlanhf_2_cu_frob,
	'zlanhf_2_CU_max': zlanhf_2_cu_max,
	'zlanhf_2_CU_one': zlanhf_2_cu_one,
	'zlanhf_2_NL_frob': zlanhf_2_nl_frob,
	'zlanhf_2_NL_max': zlanhf_2_nl_max,
	'zlanhf_2_NL_one': zlanhf_2_nl_one,
	'zlanhf_2_NU_frob': zlanhf_2_nu_frob,
	'zlanhf_2_NU_max': zlanhf_2_nu_max,
	'zlanhf_2_NU_one': zlanhf_2_nu_one,
	'zlanhf_3_CL_frob': zlanhf_3_cl_frob,
	'zlanhf_3_CL_inf': zlanhf_3_cl_inf,
	'zlanhf_3_CL_max': zlanhf_3_cl_max,
	'zlanhf_3_CL_one': zlanhf_3_cl_one,
	'zlanhf_3_CU_frob': zlanhf_3_cu_frob,
	'zlanhf_3_CU_inf': zlanhf_3_cu_inf,
	'zlanhf_3_CU_max': zlanhf_3_cu_max,
	'zlanhf_3_CU_one': zlanhf_3_cu_one,
	'zlanhf_3_NL_frob': zlanhf_3_nl_frob,
	'zlanhf_3_NL_inf': zlanhf_3_nl_inf,
	'zlanhf_3_NL_max': zlanhf_3_nl_max,
	'zlanhf_3_NL_one': zlanhf_3_nl_one,
	'zlanhf_3_NU_frob': zlanhf_3_nu_frob,
	'zlanhf_3_NU_inf': zlanhf_3_nu_inf,
	'zlanhf_3_NU_max': zlanhf_3_nu_max,
	'zlanhf_3_NU_one': zlanhf_3_nu_one,
	'zlanhf_4_CL_frob': zlanhf_4_cl_frob,
	'zlanhf_4_CL_inf': zlanhf_4_cl_inf,
	'zlanhf_4_CL_max': zlanhf_4_cl_max,
	'zlanhf_4_CL_one': zlanhf_4_cl_one,
	'zlanhf_4_CU_frob': zlanhf_4_cu_frob,
	'zlanhf_4_CU_inf': zlanhf_4_cu_inf,
	'zlanhf_4_CU_max': zlanhf_4_cu_max,
	'zlanhf_4_CU_one': zlanhf_4_cu_one,
	'zlanhf_4_NL_frob': zlanhf_4_nl_frob,
	'zlanhf_4_NL_inf': zlanhf_4_nl_inf,
	'zlanhf_4_NL_max': zlanhf_4_nl_max,
	'zlanhf_4_NL_one': zlanhf_4_nl_one,
	'zlanhf_4_NU_frob': zlanhf_4_nu_frob,
	'zlanhf_4_NU_inf': zlanhf_4_nu_inf,
	'zlanhf_4_NU_max': zlanhf_4_nu_max,
	'zlanhf_4_NU_one': zlanhf_4_nu_one,
	'zlanhf_5_CL_frob': zlanhf_5_cl_frob,
	'zlanhf_5_CL_inf': zlanhf_5_cl_inf,
	'zlanhf_5_CL_max': zlanhf_5_cl_max,
	'zlanhf_5_CL_one': zlanhf_5_cl_one,
	'zlanhf_5_CU_frob': zlanhf_5_cu_frob,
	'zlanhf_5_CU_inf': zlanhf_5_cu_inf,
	'zlanhf_5_CU_max': zlanhf_5_cu_max,
	'zlanhf_5_CU_one': zlanhf_5_cu_one,
	'zlanhf_5_NL_frob': zlanhf_5_nl_frob,
	'zlanhf_5_NL_inf': zlanhf_5_nl_inf,
	'zlanhf_5_NL_max': zlanhf_5_nl_max,
	'zlanhf_5_NL_one': zlanhf_5_nl_one,
	'zlanhf_5_NU_frob': zlanhf_5_nu_frob,
	'zlanhf_5_NU_inf': zlanhf_5_nu_inf,
	'zlanhf_5_NU_max': zlanhf_5_nu_max,
	'zlanhf_5_NU_one': zlanhf_5_nu_one,
	'zlanhf_n0': zlanhf_n0,
	'zlanhf_n1': zlanhf_n1
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

	tc = fixtures[ caseName ];
	rfpTc = fixtures[ rfpCase || caseName ];
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
