'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zlanhe = require( './../lib/base.js' );

// FIXTURES //

var zlanhe_max_u = require( './fixtures/zlanhe_max_u.json' );
var zlanhe_one_u = require( './fixtures/zlanhe_one_u.json' );
var zlanhe_one_o_u = require( './fixtures/zlanhe_one_o_u.json' );
var zlanhe_inf_u = require( './fixtures/zlanhe_inf_u.json' );
var zlanhe_frob_u = require( './fixtures/zlanhe_frob_u.json' );
var zlanhe_frob_e_u = require( './fixtures/zlanhe_frob_e_u.json' );
var zlanhe_max_l = require( './fixtures/zlanhe_max_l.json' );
var zlanhe_one_l = require( './fixtures/zlanhe_one_l.json' );
var zlanhe_inf_l = require( './fixtures/zlanhe_inf_l.json' );
var zlanhe_frob_l = require( './fixtures/zlanhe_frob_l.json' );
var zlanhe_n_zero = require( './fixtures/zlanhe_n_zero.json' );
var zlanhe_1x1_max = require( './fixtures/zlanhe_1x1_max.json' );
var zlanhe_1x1_one = require( './fixtures/zlanhe_1x1_one.json' );
var zlanhe_1x1_inf = require( './fixtures/zlanhe_1x1_inf.json' );
var zlanhe_1x1_frob = require( './fixtures/zlanhe_1x1_frob.json' );
var zlanhe_3x3_max_u = require( './fixtures/zlanhe_3x3_max_u.json' );
var zlanhe_3x3_one_u = require( './fixtures/zlanhe_3x3_one_u.json' );
var zlanhe_3x3_inf_u = require( './fixtures/zlanhe_3x3_inf_u.json' );
var zlanhe_3x3_frob_u = require( './fixtures/zlanhe_3x3_frob_u.json' );
var zlanhe_3x3_max_l = require( './fixtures/zlanhe_3x3_max_l.json' );
var zlanhe_3x3_one_l = require( './fixtures/zlanhe_3x3_one_l.json' );
var zlanhe_3x3_inf_l = require( './fixtures/zlanhe_3x3_inf_l.json' );
var zlanhe_3x3_frob_l = require( './fixtures/zlanhe_3x3_frob_l.json' );

// FUNCTIONS //

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Create a Complex128Array from interleaved re/im values in column-major layout.
* lda is the leading dimension (number of rows), n the number of columns.
* Returns array sized exactly lda*n complex elements.
*/
function makeMatrix( data, lda, n ) {
	return new Complex128Array( data );
}

// TESTS //

// 4x4 Hermitian matrix (upper triangle stored):
//   A = [  2+0i    3+1i  -1+2i   4-3i ]
//       [  3-1i    5+0i   2+0.5i -6+1i ]
//       [ -1-2i  2-0.5i   7+0i   1+4i ]
//       [  4+3i   -6-1i   1-4i   8+0i ]

// Upper triangle in column-major (LDA=4):
// col 0: (2,0), (0,0), (0,0), (0,0)
// col 1: (3,1), (5,0), (0,0), (0,0)
// col 2: (-1,2), (2,0.5), (7,0), (0,0)
// col 3: (4,-3), (-6,1), (1,4), (8,0)
var AU4 = new Complex128Array( [
	2, 0,  0, 0,  0, 0,  0, 0,
	3, 1,  5, 0,  0, 0,  0, 0,
	-1, 2,  2, 0.5,  7, 0,  0, 0,
	4, -3,  -6, 1,  1, 4,  8, 0
] );

// Lower triangle of same matrix:
var AL4 = new Complex128Array( [
	2, 0,  3, -1,  -1, -2,  4, 3,
	0, 0,  5, 0,  2, -0.5,  -6, -1,
	0, 0,  0, 0,  7, 0,  1, -4,
	0, 0,  0, 0,  0, 0,  8, 0
] );

// 3x3 Hermitian upper triangle:
var AU3 = new Complex128Array( [
	1, 0,  0, 0,  0, 0,
	2, 3,  4, 0,  0, 0,
	5, -1,  0, 6,  9, 0
] );

// 3x3 Hermitian lower triangle:
var AL3 = new Complex128Array( [
	1, 0,  2, -3,  5, 1,
	0, 0,  4, 0,  0, -6,
	0, 0,  0, 0,  9, 0
] );

test( 'zlanhe: max norm, uplo=U, 4x4', function t() {
	var tc = zlanhe_max_u;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'max', 'upper', 4, AU4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: one norm (1), uplo=U, 4x4', function t() {
	var tc = zlanhe_one_u;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'one-norm', 'upper', 4, AU4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: one norm (O), uplo=U, 4x4', function t() {
	var tc = zlanhe_one_o_u;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'one-norm', 'upper', 4, AU4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: infinity norm, uplo=U, 4x4', function t() {
	var tc = zlanhe_inf_u;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'inf-norm', 'upper', 4, AU4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: Frobenius norm (F), uplo=U, 4x4', function t() {
	var tc = zlanhe_frob_u;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'frobenius', 'upper', 4, AU4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: Frobenius norm (E), uplo=U, 4x4', function t() {
	var tc = zlanhe_frob_e_u;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'frobenius', 'upper', 4, AU4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: max norm, uplo=L, 4x4', function t() {
	var tc = zlanhe_max_l;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'max', 'lower', 4, AL4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: one norm, uplo=L, 4x4', function t() {
	var tc = zlanhe_one_l;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'one-norm', 'lower', 4, AL4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: infinity norm, uplo=L, 4x4', function t() {
	var tc = zlanhe_inf_l;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'inf-norm', 'lower', 4, AL4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: Frobenius norm, uplo=L, 4x4', function t() {
	var tc = zlanhe_frob_l;
	var work = new Float64Array( 4 );
	var result = zlanhe( 'frobenius', 'lower', 4, AL4, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: N=0 returns 0', function t() {
	var tc = zlanhe_n_zero;
	var work = new Float64Array( 4 );
	var A = new Complex128Array( 1 );
	var result = zlanhe( 'max', 'upper', 0, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 1x1 max norm', function t() {
	var tc = zlanhe_1x1_max;
	var work = new Float64Array( 1 );
	var A = new Complex128Array( [ -5.5, 0 ] );
	var result = zlanhe( 'max', 'upper', 1, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 1x1 one norm', function t() {
	var tc = zlanhe_1x1_one;
	var work = new Float64Array( 1 );
	var A = new Complex128Array( [ -5.5, 0 ] );
	var result = zlanhe( 'one-norm', 'upper', 1, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 1x1 inf norm', function t() {
	var tc = zlanhe_1x1_inf;
	var work = new Float64Array( 1 );
	var A = new Complex128Array( [ -5.5, 0 ] );
	var result = zlanhe( 'inf-norm', 'upper', 1, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 1x1 Frobenius norm', function t() {
	var tc = zlanhe_1x1_frob;
	var work = new Float64Array( 1 );
	var A = new Complex128Array( [ -5.5, 0 ] );
	var result = zlanhe( 'frobenius', 'upper', 1, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 3x3 max norm, uplo=U', function t() {
	var tc = zlanhe_3x3_max_u;
	var work = new Float64Array( 3 );
	var result = zlanhe( 'max', 'upper', 3, AU3, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 3x3 one norm, uplo=U', function t() {
	var tc = zlanhe_3x3_one_u;
	var work = new Float64Array( 3 );
	var result = zlanhe( 'one-norm', 'upper', 3, AU3, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 3x3 inf norm, uplo=U', function t() {
	var tc = zlanhe_3x3_inf_u;
	var work = new Float64Array( 3 );
	var result = zlanhe( 'inf-norm', 'upper', 3, AU3, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 3x3 Frobenius norm, uplo=U', function t() {
	var tc = zlanhe_3x3_frob_u;
	var work = new Float64Array( 3 );
	var result = zlanhe( 'frobenius', 'upper', 3, AU3, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 3x3 max norm, uplo=L', function t() {
	var tc = zlanhe_3x3_max_l;
	var work = new Float64Array( 3 );
	var result = zlanhe( 'max', 'lower', 3, AL3, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 3x3 one norm, uplo=L', function t() {
	var tc = zlanhe_3x3_one_l;
	var work = new Float64Array( 3 );
	var result = zlanhe( 'one-norm', 'lower', 3, AL3, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 3x3 inf norm, uplo=L', function t() {
	var tc = zlanhe_3x3_inf_l;
	var work = new Float64Array( 3 );
	var result = zlanhe( 'inf-norm', 'lower', 3, AL3, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhe: 3x3 Frobenius norm, uplo=L', function t() {
	var tc = zlanhe_3x3_frob_l;
	var work = new Float64Array( 3 );
	var result = zlanhe( 'frobenius', 'lower', 3, AL3, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
