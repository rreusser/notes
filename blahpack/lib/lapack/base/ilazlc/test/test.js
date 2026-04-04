/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var ilazlc = require( './../lib' );
var base = require( './../lib/base.js' );

// FIXTURES //

var ilazlc_diag = require( './fixtures/ilazlc_diag.json' );
var ilazlc_col2 = require( './fixtures/ilazlc_col2.json' );
var ilazlc_zeros = require( './fixtures/ilazlc_zeros.json' );
var ilazlc_n_zero = require( './fixtures/ilazlc_n_zero.json' );
var ilazlc_imag = require( './fixtures/ilazlc_imag.json' );
var ilazlc_full = require( './fixtures/ilazlc_full.json' );
var ilazlc_1x1 = require( './fixtures/ilazlc_1x1.json' );
var ilazlc_1x1_zero = require( './fixtures/ilazlc_1x1_zero.json' );
// Fortran returns 1-based; JS returns 0-based. Convert:
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

// For column-major complex matrix with LDA rows:
// strideA1 = 1 (each row = 1 complex element)
// strideA2 = LDA (each column = LDA complex elements)

// Helper: build Complex128Array from flat interleaved doubles
/**
* C128.
*
* @private
* @param {TypedArray} arr - input array
* @returns {*} result
*/
function c128( arr ) {
	return new Complex128Array( arr );
}

// TESTS //

test( 'ilazlc: main export is a function', function t() {
	assert.strictEqual( typeof ilazlc, 'function' );
});

test( 'ilazlc: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof ilazlc.ndarray, 'function' );
});

test( 'ilazlc: diagonal 3x3 matrix -> last non-zero column = 2 (0-based)', function t() { // eslint-disable-line max-len
	var buf;
	var tc;
	var A;

	tc = ilazlc_diag;
	buf = new Float64Array( 2 * 4 * 3 );
	buf[ 0 * 2 + 0 * 8 ] = 1.0;
	buf[ 1 * 2 + 1 * 8 ] = 2.0;
	buf[ 2 * 2 + 2 * 8 ] = 3.0;
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: last column all zeros -> returns 1 (0-based col 2)', function t() { // eslint-disable-line max-len
	var buf;
	var tc;
	var A;

	tc = ilazlc_col2;
	buf = new Float64Array( 2 * 4 * 3 );
	buf[ 0 * 2 + 0 * 8 ] = 1.0;
	buf[ 1 * 2 + 1 * 8 ] = 2.0;
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: all zeros -> returns -1', function t() {
	var buf;
	var tc;
	var A;

	tc = ilazlc_zeros;
	buf = new Float64Array( 2 * 4 * 3 );
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: N=0 -> returns -1', function t() {
	var buf;
	var tc;
	var A;

	tc = ilazlc_n_zero;
	buf = new Float64Array( 2 * 4 * 3 );
	A = c128( buf );
	assert.strictEqual( base( 3, 0, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: only imaginary part non-zero in last column', function t() {
	var buf;
	var tc;
	var A;

	tc = ilazlc_imag;
	buf = new Float64Array( 2 * 4 * 3 );
	buf[ 1 * 2 + 2 * 8 + 1 ] = 5.0;
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: full matrix -> returns N-1', function t() {
	var vals;
	var buf;
	var tc;
	var i;
	var j;
	var k;
	var A;

	tc = ilazlc_full;
	buf = new Float64Array( 2 * 4 * 3 );
	vals = [
		[1, 1],
		[2, 2],
		[3, 3],
		[4, 4],
		[5, 5],
		[6, 6],
		[7, 7],
		[8, 8],
		[9, 9]
	];
	k = 0;
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			buf[ i * 2 + j * 8 ] = vals[ k ][ 0 ];
			buf[ i * 2 + j * 8 + 1 ] = vals[ k ][ 1 ];
			k++;
		}
	}
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: 1x1 non-zero -> returns 0', function t() {
	var buf;
	var tc;
	var A;

	tc = ilazlc_1x1;
	buf = new Float64Array( 2 * 4 );
	buf[ 0 ] = 1.0;
	A = c128( buf );
	assert.strictEqual( base( 1, 1, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: 1x1 zero -> returns -1', function t() {
	var buf;
	var tc;
	var A;

	tc = ilazlc_1x1_zero;
	buf = new Float64Array( 2 * 4 );
	A = c128( buf );
	assert.strictEqual( base( 1, 1, A, 1, 4, 0 ), expected( tc ) );
});
