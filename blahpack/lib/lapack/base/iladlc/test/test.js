/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var iladlc = require( './../lib/base.js' );

// FIXTURES //

var basic_3x4 = require( './fixtures/basic_3x4.json' );
var all_zeros = require( './fixtures/all_zeros.json' );
var last_col_nonzero = require( './fixtures/last_col_nonzero.json' );
var first_col_only = require( './fixtures/first_col_only.json' );
var n_zero = require( './fixtures/n_zero.json' );
var bottom_right = require( './fixtures/bottom_right.json' );

// Fortran returns 1-based; JS returns 0-based:
/**
* Expected.
*
* @private
* @param {*} tc - tc
* @returns {*} result
*/
function expected( tc ) {
	return tc.result - 1;
}

test( 'iladlc: basic 3x4 diagonal', function t() {
	var tc = basic_3x4;

	// 3x4 col-major, LDA=4, strideA1=1, strideA2=4
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 0 * 4 ] = 1.0;
	A[ 1 + 1 * 4 ] = 2.0;
	A[ 2 + 2 * 4 ] = 3.0;
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: all zeros', function t() {
	var tc = all_zeros;
	var A = new Float64Array( 4 * 4 );
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: last column non-zero', function t() {
	var tc = last_col_nonzero;
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 3 * 4 ] = 5.0;
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: first column only', function t() {
	var tc = first_col_only;
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 0 * 4 ] = 1.0;
	A[ 1 + 0 * 4 ] = 2.0;
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: N=0', function t() {
	var tc = n_zero;
	var A = new Float64Array( 4 * 4 );
	assert.strictEqual( iladlc( 3, 0, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: bottom right corner', function t() {
	var tc = bottom_right;
	var A = new Float64Array( 4 * 4 );
	A[ 2 + 3 * 4 ] = 9.0;
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});
