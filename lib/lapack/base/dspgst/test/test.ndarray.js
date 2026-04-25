/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpptrf = require( './../../dpptrf/lib/base.js' );
var dspgst = require( './../lib/ndarray.js' );

// FIXTURES //

var itype1_upper_3 = require( './fixtures/itype1_upper_3.json' );
var itype1_lower_3 = require( './fixtures/itype1_lower_3.json' );
var itype2_upper_3 = require( './fixtures/itype2_upper_3.json' );
var itype2_lower_3 = require( './fixtures/itype2_lower_3.json' );
var itype3_upper_3 = require( './fixtures/itype3_upper_3.json' );
var itype3_lower_3 = require( './fixtures/itype3_lower_3.json' );
var itype1_upper_4 = require( './fixtures/itype1_upper_4.json' );
var itype1_lower_4 = require( './fixtures/itype1_lower_4.json' );
var itype2_upper_4 = require( './fixtures/itype2_upper_4.json' );
var itype2_lower_4 = require( './fixtures/itype2_lower_4.json' );
var itype3_upper_4 = require( './fixtures/itype3_upper_4.json' );
var itype3_lower_4 = require( './fixtures/itype3_lower_4.json' );
var n_one_itype1_upper = require( './fixtures/n_one_itype1_upper.json' );
var n_one_itype1_lower = require( './fixtures/n_one_itype1_lower.json' );
var n_one_itype2 = require( './fixtures/n_one_itype2.json' );
var n_one_itype3 = require( './fixtures/n_one_itype3.json' );

// FUNCTIONS //

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1e-14 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}

/**
* Performs Cholesky factorization of a packed SPD matrix in-place.
*
* @private
* @param {string} uplo - 'upper' or 'lower'
* @param {NonNegativeInteger} N - matrix order
* @param {Float64Array} bp - packed matrix (overwritten with factor)
*/
function cholesky( uplo, N, bp ) {
	var info = dpptrf( uplo, N, bp, 1, 0 );
	assert.equal( info, 0, 'Cholesky factorization should succeed' );
}

// TESTS //

test( 'dspgst is a function', function t() {
	assert.equal( typeof dspgst, 'function' );
});

// 3x3 tests: A = [4 2 1; 2 5 3; 1 3 6], B = [4 2 1; 2 5 1; 1 1 3] (SPD)

test( 'dspgst: itype1_upper_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype1_upper_3;
	bp = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 1.0, 3.0 ] );
	cholesky( 'upper', 3, bp );
	ap = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
	info = dspgst( 1, 'upper', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype1_lower_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype1_lower_3;
	bp = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 1.0, 3.0 ] );
	cholesky( 'lower', 3, bp );
	ap = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
	info = dspgst( 1, 'lower', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype2_upper_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype2_upper_3;
	bp = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 1.0, 3.0 ] );
	cholesky( 'upper', 3, bp );
	ap = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
	info = dspgst( 2, 'upper', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype2_lower_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype2_lower_3;
	bp = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 1.0, 3.0 ] );
	cholesky( 'lower', 3, bp );
	ap = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
	info = dspgst( 2, 'lower', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype3_upper_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype3_upper_3;
	bp = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 1.0, 3.0 ] );
	cholesky( 'upper', 3, bp );
	ap = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
	info = dspgst( 3, 'upper', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype3_lower_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype3_lower_3;
	bp = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 1.0, 3.0 ] );
	cholesky( 'lower', 3, bp );
	ap = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
	info = dspgst( 3, 'lower', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

// 4x4 tests: A = [10 4 2 1; 4 8 3 2; 2 3 7 4; 1 2 4 6]

//             B = [9 3 1 0; 3 5 2 1; 1 2 4 1; 0 1 1 3] (SPD)

test( 'dspgst: itype1_upper_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype1_upper_4;
	bp = new Float64Array( [ 9.0, 3.0, 5.0, 1.0, 2.0, 4.0, 0.0, 1.0, 1.0, 3.0 ] );
	cholesky( 'upper', 4, bp );
	ap = new Float64Array( [ 10.0, 4.0, 8.0, 2.0, 3.0, 7.0, 1.0, 2.0, 4.0, 6.0 ] );
	info = dspgst( 1, 'upper', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype1_lower_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype1_lower_4;
	bp = new Float64Array( [ 9.0, 3.0, 1.0, 0.0, 5.0, 2.0, 1.0, 4.0, 1.0, 3.0 ] );
	cholesky( 'lower', 4, bp );
	ap = new Float64Array( [ 10.0, 4.0, 2.0, 1.0, 8.0, 3.0, 2.0, 7.0, 4.0, 6.0 ] );
	info = dspgst( 1, 'lower', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype2_upper_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype2_upper_4;
	bp = new Float64Array( [ 9.0, 3.0, 5.0, 1.0, 2.0, 4.0, 0.0, 1.0, 1.0, 3.0 ] );
	cholesky( 'upper', 4, bp );
	ap = new Float64Array( [ 10.0, 4.0, 8.0, 2.0, 3.0, 7.0, 1.0, 2.0, 4.0, 6.0 ] );
	info = dspgst( 2, 'upper', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype2_lower_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype2_lower_4;
	bp = new Float64Array( [ 9.0, 3.0, 1.0, 0.0, 5.0, 2.0, 1.0, 4.0, 1.0, 3.0 ] );
	cholesky( 'lower', 4, bp );
	ap = new Float64Array( [ 10.0, 4.0, 2.0, 1.0, 8.0, 3.0, 2.0, 7.0, 4.0, 6.0 ] );
	info = dspgst( 2, 'lower', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype3_upper_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype3_upper_4;
	bp = new Float64Array( [ 9.0, 3.0, 5.0, 1.0, 2.0, 4.0, 0.0, 1.0, 1.0, 3.0 ] );
	cholesky( 'upper', 4, bp );
	ap = new Float64Array( [ 10.0, 4.0, 8.0, 2.0, 3.0, 7.0, 1.0, 2.0, 4.0, 6.0 ] );
	info = dspgst( 3, 'upper', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: itype3_lower_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = itype3_lower_4;
	bp = new Float64Array( [ 9.0, 3.0, 1.0, 0.0, 5.0, 2.0, 1.0, 4.0, 1.0, 3.0 ] );
	cholesky( 'lower', 4, bp );
	ap = new Float64Array( [ 10.0, 4.0, 2.0, 1.0, 8.0, 3.0, 2.0, 7.0, 4.0, 6.0 ] );
	info = dspgst( 3, 'lower', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

// Edge cases

test( 'dspgst: n_zero', function t() {
	var info;
	var ap;
	var bp;

	ap = new Float64Array( [ 1.0 ] );
	bp = new Float64Array( [ 1.0 ] );
	info = dspgst( 1, 'upper', 0, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dspgst: n_one_itype1_upper', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = n_one_itype1_upper;
	ap = new Float64Array( [ 9.0 ] );
	bp = new Float64Array( [ 3.0 ] );
	info = dspgst( 1, 'upper', 1, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: n_one_itype1_lower', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = n_one_itype1_lower;
	ap = new Float64Array( [ 16.0 ] );
	bp = new Float64Array( [ 4.0 ] );
	info = dspgst( 1, 'lower', 1, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: n_one_itype2', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = n_one_itype2;
	ap = new Float64Array( [ 5.0 ] );
	bp = new Float64Array( [ 2.0 ] );
	info = dspgst( 2, 'upper', 1, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});

test( 'dspgst: n_one_itype3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = n_one_itype3;
	ap = new Float64Array( [ 5.0 ] );
	bp = new Float64Array( [ 2.0 ] );
	info = dspgst( 3, 'lower', 1, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.AP, 1e-14, 'AP' );
});
