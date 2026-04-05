/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dtbcon = require( './../lib/base.js' );

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
var edge_n1_kd0 = require( './fixtures/edge_n1_kd0.json' );
var identity_onenorm = require( './fixtures/identity_onenorm.json' );
var identity_infnorm = require( './fixtures/identity_infnorm.json' );
var ill_conditioned_onenorm = require( './fixtures/ill_conditioned_onenorm.json' );
var ill_conditioned_infnorm = require( './fixtures/ill_conditioned_infnorm.json' );
var upper_nonunit_onenorm_kd3 = require( './fixtures/upper_nonunit_onenorm_kd3.json' );
var upper_nonunit_infnorm_kd3 = require( './fixtures/upper_nonunit_infnorm_kd3.json' );

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
* Creates a column-major banded matrix from a 2D array of band rows.
*
* @private
* @param {NonNegativeInteger} nrows - number of band rows (kd+1)
* @param {NonNegativeInteger} ncols - number of columns (N)
* @param {Array<Array<number>>} rows - band rows (each of length ncols)
* @returns {Float64Array} column-major band storage
*/
function bandColMajor( nrows, ncols, rows ) {
	var AB = new Float64Array( nrows * ncols );
	var i;
	var j;
	for ( i = 0; i < nrows; i++ ) {
		for ( j = 0; j < ncols; j++ ) {
			AB[ ( j * nrows ) + i ] = rows[ i ][ j ];
		}
	}
	return AB;
}

/**
* Runs dtbcon and returns { rcond, info }.
*
* @private
* @param {string} norm - 'one-norm' or 'inf-norm'
* @param {string} uplo - 'upper' or 'lower'
* @param {string} diag - 'unit' or 'non-unit'
* @param {NonNegativeInteger} n - matrix order
* @param {NonNegativeInteger} kd - bandwidth
* @param {Float64Array} AB - column-major band storage
* @param {NonNegativeInteger} ldab - leading dimension of AB (number of band rows)
* @returns {Object} result with rcond and info
*/
function run( norm, uplo, diag, n, kd, AB, ldab ) {
	var RCOND = new Float64Array( 1 );
	var IWORK = new Int32Array( Math.max( n, 1 ) );
	var WORK = new Float64Array( 3 * Math.max( n, 1 ) );
	var info;

	// Column-major: stride1=1 (consecutive rows), stride2=ldab (next column)
	info = dtbcon( norm, uplo, diag, n, kd, AB, 1, ldab, 0, RCOND, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	return {
		'rcond': RCOND[ 0 ],
		'info': info
	};
}

// VARIABLES //

// 4x4 upper triangular, kd=2:
// [ 10  -1   2   0 ]
// [  0   8  -2   1 ]
// [  0   0  12  -3 ]
// [  0   0   0   6 ]
// Band storage (3 rows x 4 cols):
// Row 0 (superdiag-2): [  0,  0,  2,  1 ]
// Row 1 (superdiag-1): [  0, -1, -2, -3 ]
// Row 2 (diagonal):    [ 10,  8, 12,  6 ]
var upperAB = bandColMajor( 3, 4, [
	[ 0, 0, 2, 1 ],
	[ 0, -1, -2, -3 ],
	[ 10, 8, 12, 6 ]
]);

// 4x4 lower triangular, kd=2:
// [  5   0   0   0 ]
// [ -2   7   0   0 ]
// [  1  -1   9   0 ]
// [  0   2  -3  11 ]
// Band storage (3 rows x 4 cols):
// Row 0 (diagonal):  [  5,  7,  9, 11 ]
// Row 1 (subdiag-1): [ -2, -1, -3,  0 ]
// Row 2 (subdiag-2): [  1,  2,  0,  0 ]
var lowerAB = bandColMajor( 3, 4, [
	[ 5, 7, 9, 11 ],
	[ -2, -1, -3, 0 ],
	[ 1, 2, 0, 0 ]
]);

// 3x3 identity, kd=1:
// Band storage (2 rows x 3 cols, upper):
// Row 0 (superdiag): [ 0, 0, 0 ]
// Row 1 (diagonal):  [ 1, 1, 1 ]
var identAB = bandColMajor( 2, 3, [
	[ 0, 0, 0 ],
	[ 1, 1, 1 ]
]);

// 3x3 ill-conditioned diagonal, kd=0:
// Band storage (1 row x 3 cols):
// Row 0 (diagonal): [ 1e12, 1, 1e-12 ]
var illAB = bandColMajor( 1, 3, [
	[ 1e12, 1, 1e-12 ]
]);

// 1x1, kd=0:
var singleAB = new Float64Array( [ 3.0 ] );

// 5x5 upper triangular, kd=3:
// [ 4  -1   2  -1   0 ]
// [ 0   6  -2   1  -1 ]
// [ 0   0   8  -3   2 ]
// [ 0   0   0   5  -2 ]
// [ 0   0   0   0   7 ]
// 0-indexed band rows:
// Row 0 (superdiag-3): [  0,  0,  0, -1,  0 ]
// Row 1 (superdiag-2): [  0,  0,  2,  1,  2 ]
// Row 2 (superdiag-1): [  0, -1, -2, -3, -2 ]
// Row 3 (diagonal):    [  4,  6,  8,  5,  7 ]
var upperABkd3 = bandColMajor( 4, 5, [
	[ 0, 0, 0, -1, 0 ],
	[ 0, 0, 2, 1, 2 ],
	[ 0, -1, -2, -3, -2 ],
	[ 4, 6, 8, 5, 7 ]
]);

// TESTS //

test( 'dtbcon: upper, non-unit, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 4, 2, upperAB, 3 );
	var tc = upper_nonunit_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtbcon: upper, non-unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 4, 2, upperAB, 3 );
	var tc = upper_nonunit_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtbcon: upper, unit, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'unit', 4, 2, upperAB, 3 );
	var tc = upper_unit_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtbcon: upper, unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'unit', 4, 2, upperAB, 3 );
	var tc = upper_unit_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtbcon: lower, non-unit, one-norm', function t() {
	var res = run( 'one-norm', 'lower', 'non-unit', 4, 2, lowerAB, 3 );
	var tc = lower_nonunit_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtbcon: lower, non-unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'lower', 'non-unit', 4, 2, lowerAB, 3 );
	var tc = lower_nonunit_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtbcon: lower, unit, one-norm', function t() {
	var res = run( 'one-norm', 'lower', 'unit', 4, 2, lowerAB, 3 );
	var tc = lower_unit_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtbcon: lower, unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'lower', 'unit', 4, 2, lowerAB, 3 );
	var tc = lower_unit_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtbcon: edge case N=0', function t() {
	var res;
	var AB;
	var tc;

	AB = new Float64Array( 0 );
	res = run( 'one-norm', 'upper', 'non-unit', 0, 0, AB, 1 );
	tc = edge_n0;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtbcon: edge case N=1, kd=0', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 1, 0, singleAB, 1 );
	var tc = edge_n1_kd0;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtbcon: identity, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 3, 1, identAB, 2 );
	var tc = identity_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtbcon: identity, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 3, 1, identAB, 2 );
	var tc = identity_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtbcon: ill-conditioned, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 3, 0, illAB, 1 );
	var tc = ill_conditioned_onenorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dtbcon: ill-conditioned, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 3, 0, illAB, 1 );
	var tc = ill_conditioned_infnorm;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dtbcon: upper, non-unit, one-norm, kd=3', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 5, 3, upperABkd3, 4 );
	var tc = upper_nonunit_onenorm_kd3;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtbcon: upper, non-unit, inf-norm, kd=3', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 5, 3, upperABkd3, 4 );
	var tc = upper_nonunit_infnorm_kd3;
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});
