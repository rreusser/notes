/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dtrcon = require( './../lib/base.js' );

// FIXTURES //

var upper_nonunit_onenorm = require( './fixtures/upper_nonunit_onenorm.json' );
var upper_nonunit_infnorm = require( './fixtures/upper_nonunit_infnorm.json' );
var upper_unit_onenorm = require( './fixtures/upper_unit_onenorm.json' );
var upper_unit_infnorm = require( './fixtures/upper_unit_infnorm.json' );
var lower_nonunit_onenorm = require( './fixtures/lower_nonunit_onenorm.json' );
var lower_nonunit_infnorm = require( './fixtures/lower_nonunit_infnorm.json' );
var lower_unit_onenorm = require( './fixtures/lower_unit_onenorm.json' );
var lower_unit_infnorm = require( './fixtures/lower_unit_infnorm.json' );
var edge_n0 = require( './fixtures/edge_n0.json' );
var edge_n1 = require( './fixtures/edge_n1.json' );
var ill_conditioned_onenorm = require( './fixtures/ill_conditioned_onenorm.json' );
var ill_conditioned_infnorm = require( './fixtures/ill_conditioned_infnorm.json' );
var identity_onenorm = require( './fixtures/identity_onenorm.json' );
var identity_infnorm = require( './fixtures/identity_infnorm.json' );

// FUNCTIONS //

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
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Creates a column-major matrix from a 2D array specification.
*
* @private
* @param {number} n - matrix dimension
* @param {Array<Array<number>>} rows - row-major 2D array
* @returns {Float64Array} column-major flat array
*/
function colMajor( n, rows ) {
	var A = new Float64Array( n * n );
	var i;
	var j;
	for ( i = 0; i < n; i++ ) {
		for ( j = 0; j < n; j++ ) {
			A[ j * n + i ] = rows[ i ][ j ];
		}
	}
	return A;
}

/**
* Runs dtrcon and returns { rcond, info }.
*
* @private
* @param {string} norm - 'one-norm' or 'inf-norm'
* @param {string} uplo - 'upper' or 'lower'
* @param {string} diag - 'unit' or 'non-unit'
* @param {number} n - matrix order
* @param {Float64Array} A - column-major matrix
* @returns {Object} result with rcond and info
*/
function run( norm, uplo, diag, n, A ) {
	var RCOND = new Float64Array( 1 );
	var IWORK = new Int32Array( Math.max( n, 1 ) );
	var WORK = new Float64Array( 3 * Math.max( n, 1 ) );
	var info;

	info = dtrcon( norm, uplo, diag, n, A, 1, n, 0, RCOND, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	return {
		'rcond': RCOND[ 0 ],
		'info': info
	};
}

// FIXTURES DATA //

// Well-conditioned upper triangular 4x4:
//   [ 10  -1   2  -1 ]
//   [  0   8  -2   1 ]
//   [  0   0  12  -3 ]
//   [  0   0   0   6 ]
var upperA = colMajor( 4, [
	[ 10, -1, 2, -1 ],
	[ 0, 8, -2, 1 ],
	[ 0, 0, 12, -3 ],
	[ 0, 0, 0, 6 ]
]);

// Well-conditioned lower triangular 4x4:

//   [  5   0   0   0 ]

//   [ -2   7   0   0 ]

//   [  1  -1   9   0 ]

//   [ -1   2  -3  11 ]
var lowerA = colMajor( 4, [
	[ 5, 0, 0, 0 ],
	[ -2, 7, 0, 0 ],
	[ 1, -1, 9, 0 ],
	[ -1, 2, -3, 11 ]
]);

// Ill-conditioned diagonal 3x3:
var illA = colMajor( 3, [
	[ 1e12, 0, 0 ],
	[ 0, 1, 0 ],
	[ 0, 0, 1e-12 ]
]);

// Identity 3x3:
var identA = colMajor( 3, [
	[ 1, 0, 0 ],
	[ 0, 1, 0 ],
	[ 0, 0, 1 ]
]);

// Single element for N=1 test:
var singleA = new Float64Array( [ 3.0 ] );

// TESTS //

test( 'dtrcon: upper, non-unit, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 4, upperA );
	var tc = upper_nonunit_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtrcon: upper, non-unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 4, upperA );
	var tc = upper_nonunit_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtrcon: upper, unit, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'unit', 4, upperA );
	var tc = upper_unit_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtrcon: upper, unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'unit', 4, upperA );
	var tc = upper_unit_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtrcon: lower, non-unit, one-norm', function t() {
	var res = run( 'one-norm', 'lower', 'non-unit', 4, lowerA );
	var tc = lower_nonunit_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtrcon: lower, non-unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'lower', 'non-unit', 4, lowerA );
	var tc = lower_nonunit_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtrcon: lower, unit, one-norm', function t() {
	var res = run( 'one-norm', 'lower', 'unit', 4, lowerA );
	var tc = lower_unit_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtrcon: lower, unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'lower', 'unit', 4, lowerA );
	var tc = lower_unit_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtrcon: edge case N=0', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 0, new Float64Array( 0 ) );
	var tc = edge_n0;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtrcon: edge case N=1', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 1, singleA );
	var tc = edge_n1;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtrcon: ill-conditioned, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 3, illA );
	var tc = ill_conditioned_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dtrcon: ill-conditioned, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 3, illA );
	var tc = ill_conditioned_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dtrcon: identity, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 3, identA );
	var tc = identity_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtrcon: identity, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 3, identA );
	var tc = identity_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});
