/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var iladlr = require( './../lib/ndarray.js' );

// FIXTURES //

var basic_3x4 = require( './fixtures/basic_3x4.json' );
var all_zeros = require( './fixtures/all_zeros.json' );
var last_row_nonzero = require( './fixtures/last_row_nonzero.json' );
var first_row_only = require( './fixtures/first_row_only.json' );
var m_zero = require( './fixtures/m_zero.json' );
var bottom_right = require( './fixtures/bottom_right.json' );

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

test( 'iladlr: basic 3x4 diagonal', function t() {
	var tc = basic_3x4;
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 0 * 4 ] = 1.0;
	A[ 1 + 1 * 4 ] = 2.0;
	A[ 2 + 2 * 4 ] = 3.0;
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: all zeros', function t() {
	var tc = all_zeros;
	var A = new Float64Array( 4 * 4 );
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: last row non-zero', function t() {
	var tc = last_row_nonzero;
	var A = new Float64Array( 4 * 4 );
	A[ 2 + 0 * 4 ] = 5.0;
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: first row only', function t() {
	var tc = first_row_only;
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 0 * 4 ] = 1.0;
	A[ 0 + 2 * 4 ] = 2.0;
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: M=0', function t() {
	var tc = m_zero;
	var A = new Float64Array( 4 * 4 );
	assert.strictEqual( iladlr( 0, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: bottom right corner', function t() {
	var tc = bottom_right;
	var A = new Float64Array( 4 * 4 );
	A[ 2 + 3 * 4 ] = 9.0;
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});
