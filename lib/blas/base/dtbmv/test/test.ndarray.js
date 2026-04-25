/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtbmv = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_notrans_nonunit = require( './fixtures/upper_notrans_nonunit.json' );
var upper_trans_nonunit = require( './fixtures/upper_trans_nonunit.json' );
var upper_notrans_unit = require( './fixtures/upper_notrans_unit.json' );
var lower_notrans_nonunit = require( './fixtures/lower_notrans_nonunit.json' );
var lower_trans_nonunit = require( './fixtures/lower_trans_nonunit.json' );
var stride = require( './fixtures/stride.json' );

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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

// TESTS //

// Upper triangular band matrix (K=1, LDA=2):
//   [ 2  3  0  0 ]  band: row0=superdiag, row1=diag
//   [ 0  4  5  0 ]
//   [ 0  0  6  7 ]
//   [ 0  0  0  8 ]
// Band storage (column-major, LDA=2): [0,2, 3,4, 5,6, 7,8]

// Lower triangular band matrix (K=1, LDA=2):
//   [ 2  0  0  0 ]  band: row0=diag, row1=subdiag
//   [ 3  4  0  0 ]
//   [ 0  5  6  0 ]
//   [ 0  0  7  8 ]
// Band storage (column-major, LDA=2): [2,3, 4,5, 6,7, 8,0]

test( 'dtbmv: upper_notrans_nonunit', function t() {
	var tc = upper_notrans_nonunit;
	var A = new Float64Array( [ 0, 2, 3, 4, 5, 6, 7, 8 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtbmv( 'upper', 'no-transpose', 'non-unit', 4, 1, A, 1, 2, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtbmv: upper_trans_nonunit', function t() {
	var tc = upper_trans_nonunit;
	var A = new Float64Array( [ 0, 2, 3, 4, 5, 6, 7, 8 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtbmv( 'upper', 'transpose', 'non-unit', 4, 1, A, 1, 2, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtbmv: upper_notrans_unit', function t() {
	var tc = upper_notrans_unit;
	var A = new Float64Array( [ 0, 2, 3, 4, 5, 6, 7, 8 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtbmv( 'upper', 'no-transpose', 'unit', 4, 1, A, 1, 2, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtbmv: lower_notrans_nonunit', function t() {
	var tc = lower_notrans_nonunit;
	var A = new Float64Array( [ 2, 3, 4, 5, 6, 7, 8, 0 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtbmv( 'lower', 'no-transpose', 'non-unit', 4, 1, A, 1, 2, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtbmv: lower_trans_nonunit', function t() {
	var tc = lower_trans_nonunit;
	var A = new Float64Array( [ 2, 3, 4, 5, 6, 7, 8, 0 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtbmv( 'lower', 'transpose', 'non-unit', 4, 1, A, 1, 2, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtbmv: n_zero', function t() {
	var x = new Float64Array( [ 99.0 ] );
	var A = new Float64Array( [ 2, 3, 4, 5, 6, 7, 8, 0 ] );
	dtbmv( 'upper', 'no-transpose', 'non-unit', 0, 1, A, 1, 2, 0, x, 1, 0 );
	assert.equal( x[ 0 ], 99.0 );
});

test( 'dtbmv: stride', function t() {
	var tc = stride;
	var A = new Float64Array( [ 0, 2, 3, 4, 5, 6, 7, 8 ] );
	var x = new Float64Array( [ 1, 0, 2, 0, 3, 0, 4, 0 ] );
	dtbmv( 'upper', 'no-transpose', 'non-unit', 4, 1, A, 1, 2, 0, x, 2, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});
