/* eslint-disable max-len, max-lines, max-lines-per-function, max-statements */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zla_porpvgrw = require( './../lib/base.js' ); // eslint-disable-line camelcase
var ndarray = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_3x3 = require( './fixtures/upper_3x3.json' );
var lower_3x3 = require( './fixtures/lower_3x3.json' );
var ncols_0 = require( './fixtures/ncols_0.json' );
var ncols_1_upper = require( './fixtures/ncols_1_upper.json' );
var ncols_1_lower = require( './fixtures/ncols_1_lower.json' );
var upper_4x4 = require( './fixtures/upper_4x4.json' );
var lower_4x4 = require( './fixtures/lower_4x4.json' );
var upper_zero_col = require( './fixtures/upper_zero_col.json' );
var upper_rpvgrw_lt1 = require( './fixtures/upper_rpvgrw_lt1.json' );
var lower_rpvgrw_lt1 = require( './fixtures/lower_rpvgrw_lt1.json' );
var lower_zero_col = require( './fixtures/lower_zero_col.json' );

// FUNCTIONS //

/**
* Asserts that two numbers are close within a relative tolerance.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - relative tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are close within a relative tolerance.
*
* @private
* @param {Array} actual - actual array
* @param {Array} expected - expected array
* @param {number} tol - relative tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

// TESTS //

test( 'zla_porpvgrw is a function', function t() {
	assert.equal( typeof zla_porpvgrw, 'function' );
});

test( 'ndarray is a function', function t() {
	assert.equal( typeof ndarray, 'function' );
});

test( 'ndarray throws if uplo is invalid', function t() {
	assert.throws( function invalid() {
		ndarray( 'foo', 1, new Complex128Array( 1 ), 1, 1, 0, new Complex128Array( 1 ), 1, 1, 0, new Float64Array( 2 ), 1, 0 ); // eslint-disable-line max-len
	}, /TypeError/ );
});

test( 'zla_porpvgrw: upper_3x3', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = upper_3x3;
	ncols = 3;
	lda = 4;

	// Upper triangle of 3x3 Hermitian PD matrix, LDA=4, column-major
	A = new Complex128Array( [
		4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		2.0, 1.0, 5.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		-2.0, 0.5, 1.0, 0.3, 14.0, 0.0, 0.0, 0.0
	] );

	// zpotrf('U') Cholesky factor (hand-computed):
	// U(1,1) = 2+0i, U(1,2) = 1+0.5i, U(2,2) = sqrt(3.75)+0i
	// U(1,3) = -1+0.25i, U(2,3) = (1.875-0.45i)/sqrt(3.75)
	// U(3,3) = sqrt(14 - 1.0625 - |U(2,3)|^2)+0i
	AF = new Complex128Array( [
		2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		1.0, 0.5, Math.sqrt( 3.75 ), 0.0, 0.0, 0.0, 0.0, 0.0,
		-1.0, 0.25, 1.875 / Math.sqrt( 3.75 ), -0.45 / Math.sqrt( 3.75 ), Math.sqrt( 14.0 - 1.0625 - ( ( 1.875 * 1.875 + 0.45 * 0.45 ) / 3.75 ) ), 0.0, 0.0, 0.0
	] );

	WORK = new Float64Array( 2 * ncols );
	result = zla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'zla_porpvgrw: lower_3x3', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = lower_3x3;
	ncols = 3;
	lda = 4;

	// Lower triangle of same Hermitian PD matrix (conjugate-transposed storage)
	A = new Complex128Array( [
		4.0, 0.0, 2.0, -1.0, -2.0, -0.5, 0.0, 0.0,
		0.0, 0.0, 5.0, 0.0, 1.0, -0.3, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 14.0, 0.0, 0.0, 0.0
	] );

	// L = conj(U^T): L(i,j) = conj(U(j,i))
	AF = new Complex128Array( [
		2.0, 0.0, 1.0, -0.5, -1.0, -0.25, 0.0, 0.0,
		0.0, 0.0, Math.sqrt( 3.75 ), 0.0, 1.875 / Math.sqrt( 3.75 ), 0.45 / Math.sqrt( 3.75 ), 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, Math.sqrt( 14.0 - 1.0625 - ( ( 1.875 * 1.875 + 0.45 * 0.45 ) / 3.75 ) ), 0.0, 0.0, 0.0
	] );

	WORK = new Float64Array( 2 * ncols );
	result = zla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'zla_porpvgrw: ncols_0', function t() {
	var result;
	var WORK;
	var tc;
	var AF;
	var A;

	tc = ncols_0;
	A = new Complex128Array( 1 );
	AF = new Complex128Array( 1 );
	WORK = new Float64Array( 0 );
	result = zla_porpvgrw( 'upper', 0, A, 1, 1, 0, AF, 1, 1, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zla_porpvgrw: ncols_1_upper', function t() {
	var result;
	var WORK;
	var tc;
	var AF;
	var A;

	tc = ncols_1_upper;
	A = new Complex128Array( [ 9.0, 0.0 ] );
	AF = new Complex128Array( [ 3.0, 0.0 ] );
	WORK = new Float64Array( 2 );
	result = zla_porpvgrw( 'upper', 1, A, 1, 1, 0, AF, 1, 1, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'zla_porpvgrw: ncols_1_lower', function t() {
	var result;
	var WORK;
	var tc;
	var AF;
	var A;

	tc = ncols_1_lower;
	A = new Complex128Array( [ 9.0, 0.0 ] );
	AF = new Complex128Array( [ 3.0, 0.0 ] );
	WORK = new Float64Array( 2 );
	result = zla_porpvgrw( 'lower', 1, A, 1, 1, 0, AF, 1, 1, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'zla_porpvgrw: upper_4x4', function t() {
	var result;
	var ncols;
	var WORK;
	var u12r;
	var u12i;
	var u13r;
	var u13i;
	var u14r;
	var u14i;
	var u23r;
	var u23i;
	var u24r;
	var u24i;
	var u34r;
	var u34i;
	var s10;
	var u22;
	var u33;
	var u44;
	var lda;
	var t1r;
	var t1i;
	var t2r;
	var t2i;
	var cr;
	var ci;
	var tc;
	var AF;
	var A;

	tc = upper_4x4;
	ncols = 4;
	lda = 4;
	s10 = Math.sqrt( 10.0 );

	// Upper triangle of 4x4 Hermitian PD matrix
	A = new Complex128Array( [
		10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		1.0, 0.5, 10.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		2.0, -0.3, 1.0, 0.2, 10.0, 0.0, 0.0, 0.0,
		0.5, 0.1, 1.5, -0.4, 2.0, 0.7, 10.0, 0.0
	] );

	// Compute zpotrf('U') Cholesky factor
	u12r = 1.0 / s10;
	u12i = 0.5 / s10;
	u22 = Math.sqrt( 10.0 - u12r * u12r - u12i * u12i );
	u13r = 2.0 / s10;
	u13i = -0.3 / s10;
	cr = u12r * u13r + u12i * u13i;
	ci = u12r * u13i - u12i * u13r;
	u23r = ( 1.0 - cr ) / u22;
	u23i = ( 0.2 - ci ) / u22;
	u33 = Math.sqrt( 10.0 - ( u13r * u13r + u13i * u13i ) - ( u23r * u23r + u23i * u23i ) );
	u14r = 0.5 / s10;
	u14i = 0.1 / s10;
	cr = u12r * u14r + u12i * u14i;
	ci = u12r * u14i - u12i * u14r;
	u24r = ( 1.5 - cr ) / u22;
	u24i = ( -0.4 - ci ) / u22;
	t1r = u13r * u14r + u13i * u14i;
	t1i = u13r * u14i - u13i * u14r;
	t2r = u23r * u24r + u23i * u24i;
	t2i = u23r * u24i - u23i * u24r;
	u34r = ( 2.0 - t1r - t2r ) / u33;
	u34i = ( 0.7 - t1i - t2i ) / u33;
	u44 = Math.sqrt( 10.0 - ( u14r * u14r + u14i * u14i ) - ( u24r * u24r + u24i * u24i ) - ( u34r * u34r + u34i * u34i ) );

	AF = new Complex128Array( [
		s10, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		u12r, u12i, u22, 0.0, 0.0, 0.0, 0.0, 0.0,
		u13r, u13i, u23r, u23i, u33, 0.0, 0.0, 0.0,
		u14r, u14i, u24r, u24i, u34r, u34i, u44, 0.0
	] );

	WORK = new Float64Array( 2 * ncols );
	result = zla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'zla_porpvgrw: lower_4x4', function t() {
	var result;
	var ncols;
	var WORK;
	var u12r;
	var u12i;
	var u13r;
	var u13i;
	var u14r;
	var u14i;
	var u23r;
	var u23i;
	var u24r;
	var u24i;
	var u34r;
	var u34i;
	var s10;
	var u22;
	var u33;
	var u44;
	var lda;
	var t1r;
	var t1i;
	var t2r;
	var t2i;
	var cr;
	var ci;
	var tc;
	var AF;
	var A;

	tc = lower_4x4;
	ncols = 4;
	lda = 4;
	s10 = Math.sqrt( 10.0 );

	// Lower triangle of same 4x4 Hermitian PD matrix
	A = new Complex128Array( [
		10.0, 0.0, 1.0, -0.5, 2.0, 0.3, 0.5, -0.1,
		0.0, 0.0, 10.0, 0.0, 1.0, -0.2, 1.5, 0.4,
		0.0, 0.0, 0.0, 0.0, 10.0, 0.0, 2.0, -0.7,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.0, 0.0
	] );

	// L = conj(U^T): L(i,j) = conj(U(j,i))
	u12r = 1.0 / s10;
	u12i = 0.5 / s10;
	u22 = Math.sqrt( 10.0 - u12r * u12r - u12i * u12i );
	u13r = 2.0 / s10;
	u13i = -0.3 / s10;
	cr = u12r * u13r + u12i * u13i;
	ci = u12r * u13i - u12i * u13r;
	u23r = ( 1.0 - cr ) / u22;
	u23i = ( 0.2 - ci ) / u22;
	u33 = Math.sqrt( 10.0 - ( u13r * u13r + u13i * u13i ) - ( u23r * u23r + u23i * u23i ) );
	u14r = 0.5 / s10;
	u14i = 0.1 / s10;
	cr = u12r * u14r + u12i * u14i;
	ci = u12r * u14i - u12i * u14r;
	u24r = ( 1.5 - cr ) / u22;
	u24i = ( -0.4 - ci ) / u22;
	t1r = u13r * u14r + u13i * u14i;
	t1i = u13r * u14i - u13i * u14r;
	t2r = u23r * u24r + u23i * u24i;
	t2i = u23r * u24i - u23i * u24r;
	u34r = ( 2.0 - t1r - t2r ) / u33;
	u34i = ( 0.7 - t1i - t2i ) / u33;
	u44 = Math.sqrt( 10.0 - ( u14r * u14r + u14i * u14i ) - ( u24r * u24r + u24i * u24i ) - ( u34r * u34r + u34i * u34i ) );

	AF = new Complex128Array( [
		s10, 0.0, u12r, -u12i, u13r, -u13i, u14r, -u14i,
		0.0, 0.0, u22, 0.0, u23r, -u23i, u24r, -u24i,
		0.0, 0.0, 0.0, 0.0, u33, 0.0, u34r, -u34i,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, u44, 0.0
	] );

	WORK = new Float64Array( 2 * ncols );
	result = zla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'zla_porpvgrw: upper_zero_col', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = upper_zero_col;
	ncols = 3;
	lda = 4;

	A = new Complex128Array( [
		4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		2.0, 1.0, 5.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		1.0, 0.5, 3.0, -0.2, 7.0, 0.0, 0.0, 0.0
	] );

	// AF with zero column 2 (upper tri)
	AF = new Complex128Array( [
		2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		1.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		0.5, 0.3, 0.0, 0.0, 2.5, 0.0, 0.0, 0.0
	] );

	WORK = new Float64Array( 2 * ncols );
	result = zla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'zla_porpvgrw: upper_rpvgrw_lt1', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = upper_rpvgrw_lt1;
	ncols = 3;
	lda = 4;

	// A: upper triangle with small elements
	A = new Complex128Array( [
		2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		1.0, 0.1, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		0.5, 0.2, 1.0, -0.1, 4.0, 0.0, 0.0, 0.0
	] );

	// AF: upper triangle with larger elements
	AF = new Complex128Array( [
		4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		2.0, 0.3, 6.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		1.0, -0.5, 3.0, 0.4, 8.0, 0.0, 0.0, 0.0
	] );

	WORK = new Float64Array( 2 * ncols );
	result = zla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'zla_porpvgrw: lower_rpvgrw_lt1', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = lower_rpvgrw_lt1;
	ncols = 3;
	lda = 4;

	// A: lower triangle with small elements
	A = new Complex128Array( [
		2.0, 0.0, 1.0, -0.1, 0.5, -0.2, 0.0, 0.0,
		0.0, 0.0, 3.0, 0.0, 1.0, 0.1, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 4.0, 0.0, 0.0, 0.0
	] );

	// AF: lower triangle with larger elements
	AF = new Complex128Array( [
		4.0, 0.0, 2.0, -0.3, 1.0, 0.5, 0.0, 0.0,
		0.0, 0.0, 6.0, 0.0, 3.0, -0.4, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 8.0, 0.0, 0.0, 0.0
	] );

	WORK = new Float64Array( 2 * ncols );
	result = zla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'zla_porpvgrw: lower_zero_col', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = lower_zero_col;
	ncols = 3;
	lda = 4;

	A = new Complex128Array( [
		4.0, 0.0, 2.0, -1.0, 1.0, -0.5, 0.0, 0.0,
		0.0, 0.0, 5.0, 0.0, 3.0, 0.2, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 7.0, 0.0, 0.0, 0.0
	] );

	// AF with zero column 2 (lower tri)
	AF = new Complex128Array( [
		2.0, 0.0, 1.0, -0.5, 0.5, 0.3, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 2.5, 0.0, 0.0, 0.0
	] );

	WORK = new Float64Array( 2 * ncols );
	result = zla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});
