/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '../../dsptrf/lib/base.js' );
var dspcon = require( './../lib/base.js' );

// FIXTURES //

var upper_well_cond = require( './fixtures/upper_well_cond.json' );
var lower_well_cond = require( './fixtures/lower_well_cond.json' );
var identity_upper = require( './fixtures/identity_upper.json' );
var identity_lower = require( './fixtures/identity_lower.json' );
var ill_cond_upper = require( './fixtures/ill_cond_upper.json' );
var n_one_upper = require( './fixtures/n_one_upper.json' );
var _4x4_upper = require( './fixtures/4x4_upper.json' );
var _4x4_lower = require( './fixtures/4x4_lower.json' );

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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
}

/**
* Factorizes and estimates the reciprocal condition number for a packed symmetric matrix.
*
* @private
* @param {string} uplo - 'upper' or 'lower'
* @param {NonNegativeInteger} N - order of the matrix
* @param {Array} apArr - packed matrix elements
* @param {number} anorm - the 1-norm of the original matrix
* @returns {Object} result with rcond, info
*/
function computeRcond( uplo, N, apArr, anorm ) {
	var rcond = new Float64Array( 1 );
	var iwork = new Int32Array( N );
	var work = new Float64Array( 2 * N );
	var ipiv = new Int32Array( N );
	var info;
	var AP = new Float64Array( apArr );

	dsptrf( uplo, N, AP, 1, 0, ipiv, 1, 0 );
	info = dspcon( uplo, N, AP, 1, 0, ipiv, 1, 0, anorm, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	return {
		'rcond': rcond[ 0 ],
		'info': info
	};
}

// TESTS //

test( 'dspcon: main export is a function', function t() {
	assert.strictEqual( typeof dspcon, 'function' );
});

test( 'dspcon: well-conditioned 3x3 (upper, packed)', function t() {
	var result;
	var tc;

	tc = upper_well_cond;
	result = computeRcond( 'upper', 3, [ 4, 1, 3, 1, 1, 2 ], tc.anorm );
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dspcon: well-conditioned 3x3 (lower, packed)', function t() {
	var result;
	var tc;

	tc = lower_well_cond;
	result = computeRcond( 'lower', 3, [ 4, 1, 1, 3, 1, 2 ], tc.anorm );
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dspcon: identity 3x3 (upper, packed, rcond=1)', function t() {
	var result;
	var tc;

	tc = identity_upper;
	result = computeRcond( 'upper', 3, [ 1, 0, 1, 0, 0, 1 ], tc.anorm );
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dspcon: identity 3x3 (lower, packed, rcond=1)', function t() {
	var result;
	var tc;

	tc = identity_lower;
	result = computeRcond( 'lower', 3, [ 1, 0, 0, 1, 0, 1 ], tc.anorm );
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dspcon: ill-conditioned 3x3 (upper, packed)', function t() {
	var result;
	var tc;

	tc = ill_cond_upper;
	result = computeRcond( 'upper', 3, [ 1, 0, 1, 0, 0, 1e-15 ], tc.anorm );
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dspcon: singular 3x3 (upper, packed, rcond=0)', function t() {
	// A = [[1, 2, 3], [2, 4, 6], [3, 6, 9]] (rank 1)
	// Upper packed: 1, 2, 4, 3, 6, 9
	var result = computeRcond( 'upper', 3, [ 1, 2, 4, 3, 6, 9 ], 18.0 );
	assert.strictEqual( result.info, 0 );
	assert.strictEqual( result.rcond, 0.0 );
});

test( 'dspcon: N=0 (rcond=1)', function t() {
	var iwork;
	var rcond;
	var ipiv;
	var work;
	var info;
	var AP;

	AP = new Float64Array( 1 );
	ipiv = new Int32Array( 1 );
	work = new Float64Array( 1 );
	iwork = new Int32Array( 1 );
	rcond = new Float64Array( 1 );
	info = dspcon( 'upper', 0, AP, 1, 0, ipiv, 1, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 1.0 );
});

test( 'dspcon: N=1 (upper, packed)', function t() {
	var result;
	var tc;

	tc = n_one_upper;
	result = computeRcond( 'upper', 1, [ 5.0 ], tc.anorm );
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dspcon: 4x4 (upper, packed)', function t() {
	var result;
	var tc;

	tc = _4x4_upper;
	result = computeRcond( 'upper', 4, [ 10, 1, 8, 2, 1, 6, 0, 1, 1, 5 ], tc.anorm ); // eslint-disable-line max-len
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dspcon: 4x4 (lower, packed)', function t() {
	var result;
	var tc;

	tc = _4x4_lower;
	result = computeRcond( 'lower', 4, [ 10, 1, 2, 0, 8, 1, 1, 6, 1, 5 ], tc.anorm ); // eslint-disable-line max-len
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dspcon: singular 3x3 (lower, packed, rcond=0)', function t() {
	// A = [[1, 2, 3], [2, 4, 6], [3, 6, 9]] (rank 1)
	// Lower packed: A(1,1)=1, A(2,1)=2, A(3,1)=3, A(2,2)=4, A(3,2)=6, A(3,3)=9
	var result = computeRcond( 'lower', 3, [ 1, 2, 3, 4, 6, 9 ], 18.0 );
	assert.strictEqual( result.info, 0 );
	assert.strictEqual( result.rcond, 0.0 );
});

test( 'dspcon: anorm=0 returns rcond=0', function t() {
	// 2x2 identity packed upper: 1, 0, 1
	var result = computeRcond( 'upper', 2, [ 1, 0, 1 ], 0.0 );
	assert.strictEqual( result.info, 0 );
	assert.strictEqual( result.rcond, 0.0 );
});

test( 'dspcon: anorm negative returns rcond=0', function t() {
	var result = computeRcond( 'upper', 2, [ 1, 0, 1 ], -1.0 );
	assert.strictEqual( result.info, 0 );
	assert.strictEqual( result.rcond, 0.0 );
});
