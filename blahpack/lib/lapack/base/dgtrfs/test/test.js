/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgttrf = require( './../../dgttrf/lib/base.js' );
var dgttrs = require( './../../dgttrs/lib/base.js' );
var dgtrfs = require( './../lib/base.js' );

// FIXTURES //

var basic_notrans = require( './fixtures/basic_notrans.json' );
var basic_trans = require( './fixtures/basic_trans.json' );
var multi_rhs_notrans = require( './fixtures/multi_rhs_notrans.json' );
var n_one = require( './fixtures/n_one.json' );
var n_zero = require( './fixtures/n_zero.json' );
var pivot_5x5_notrans = require( './fixtures/pivot_5x5_notrans.json' );

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

/**
* ToF64.
*
* @private
* @param {TypedArray} arr - input array
* @returns {*} result
*/
function toF64( arr ) {
	return new Float64Array( arr );
}

/**
* ToI32.
*
* @private
* @param {TypedArray} arr - input array
* @returns {*} result
*/
function toI32( arr ) {
	return new Int32Array( arr );
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

// TESTS //

test( 'dgtrfs: basic_notrans', function t() {
	var iwork;
	var ipiv;
	var ferr;
	var berr;
	var work;
	var info;
	var dlf;
	var duf;
	var du2;
	var tc;
	var dl;
	var du;
	var df;
	var N;
	var d;
	var b;
	var x;

	tc = basic_notrans;
	N = 4;
	dl = toF64( [ 3.0, 1.0, 2.0 ] );
	d = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	du = toF64( [ -1.0, -2.0, -3.0 ] );
	dlf = toF64( [ 3.0, 1.0, 2.0 ] );
	df = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	duf = toF64( [ -1.0, -2.0, -3.0 ] );
	du2 = new Float64Array( 2 );
	ipiv = new Int32Array( 4 );
	dgttrf( N, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0 );
	b = new Float64Array( N );
	b[ 0 ] = 0.0;
	b[ 1 ] = 5.0;
	b[ 2 ] = 5.0;
	b[ 3 ] = 30.0;
	x = new Float64Array( N );
	x[ 0 ] = b[ 0 ];
	x[ 1 ] = b[ 1 ];
	x[ 2 ] = b[ 2 ];
	x[ 3 ] = b[ 3 ];
	dgttrs( 'no-transpose', N, 1, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, x, 1, N, 0 ); // eslint-disable-line max-len
	ferr = new Float64Array( 1 );
	berr = new Float64Array( 1 );
	work = new Float64Array( 3 * N );
	iwork = new Int32Array( N );
	info = dgtrfs( 'no-transpose', N, 1, dl, 1, 0, d, 1, 0, du, 1, 0, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, b, 1, N, 0, x, 1, N, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0, iwork, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'dgtrfs: basic_trans', function t() {
	var iwork;
	var ipiv;
	var ferr;
	var berr;
	var work;
	var info;
	var dlf;
	var duf;
	var du2;
	var tc;
	var dl;
	var du;
	var df;
	var N;
	var d;
	var b;
	var x;

	tc = basic_trans;
	N = 4;
	dl = toF64( [ 3.0, 1.0, 2.0 ] );
	d = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	du = toF64( [ -1.0, -2.0, -3.0 ] );
	dlf = toF64( [ 3.0, 1.0, 2.0 ] );
	df = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	duf = toF64( [ -1.0, -2.0, -3.0 ] );
	du2 = new Float64Array( 2 );
	ipiv = new Int32Array( 4 );
	dgttrf( N, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0 );
	b = toF64( [ 8.0, 10.0, 19.0, 15.0 ] );
	x = toF64( [ 8.0, 10.0, 19.0, 15.0 ] );
	dgttrs( 'transpose', N, 1, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, x, 1, N, 0 ); // eslint-disable-line max-len
	ferr = new Float64Array( 1 );
	berr = new Float64Array( 1 );
	work = new Float64Array( 3 * N );
	iwork = new Int32Array( N );
	info = dgtrfs( 'transpose', N, 1, dl, 1, 0, d, 1, 0, du, 1, 0, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, b, 1, N, 0, x, 1, N, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0, iwork, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'dgtrfs: multi_rhs_notrans', function t() {
	var iwork;
	var nrhs;
	var ipiv;
	var ferr;
	var berr;
	var work;
	var info;
	var dlf;
	var duf;
	var du2;
	var tc;
	var dl;
	var du;
	var df;
	var N;
	var d;
	var b;
	var x;

	tc = multi_rhs_notrans;
	N = 4;
	nrhs = 2;
	dl = toF64( [ 3.0, 1.0, 2.0 ] );
	d = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	du = toF64( [ -1.0, -2.0, -3.0 ] );
	dlf = toF64( [ 3.0, 1.0, 2.0 ] );
	df = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	duf = toF64( [ -1.0, -2.0, -3.0 ] );
	du2 = new Float64Array( 2 );
	ipiv = new Int32Array( 4 );
	dgttrf( N, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0 );
	b = toF64( [ 0.0, 5.0, 5.0, 30.0, 4.0, 4.0, -4.0, 20.0 ] );
	x = toF64( [ 0.0, 5.0, 5.0, 30.0, 4.0, 4.0, -4.0, 20.0 ] );
	dgttrs( 'no-transpose', N, nrhs, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, x, 1, N, 0 ); // eslint-disable-line max-len
	ferr = new Float64Array( 2 );
	berr = new Float64Array( 2 );
	work = new Float64Array( 3 * N );
	iwork = new Int32Array( N );
	info = dgtrfs( 'no-transpose', N, nrhs, dl, 1, 0, d, 1, 0, du, 1, 0, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, b, 1, N, 0, x, 1, N, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0, iwork, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( x.subarray( 0, 4 ) ), tc.x1, 1e-14, 'x1' );
	assertArrayClose( toArray( x.subarray( 4, 8 ) ), tc.x2, 1e-14, 'x2' );
});

test( 'dgtrfs: n_one', function t() {
	var iwork;
	var ipiv;
	var ferr;
	var berr;
	var work;
	var info;
	var dlf;
	var duf;
	var du2;
	var tc;
	var dl;
	var du;
	var df;
	var d;
	var b;
	var x;

	tc = n_one;
	dl = new Float64Array( 0 );
	d = toF64( [ 5.0 ] );
	du = new Float64Array( 0 );
	dlf = new Float64Array( 0 );
	df = toF64( [ 5.0 ] );
	duf = new Float64Array( 0 );
	du2 = new Float64Array( 0 );
	ipiv = new Int32Array( [ 0 ] );
	b = toF64( [ 10.0 ] );
	x = toF64( [ 2.0 ] );
	ferr = new Float64Array( 1 );
	berr = new Float64Array( 1 );
	work = new Float64Array( 3 );
	iwork = new Int32Array( 1 );
	info = dgtrfs( 'no-transpose', 1, 1, dl, 1, 0, d, 1, 0, du, 1, 0, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, b, 1, 1, 0, x, 1, 1, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0, iwork, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'dgtrfs: n_zero', function t() {
	var iwork;
	var ferr;
	var berr;
	var work;
	var ipiv;
	var info;
	var dlf;
	var duf;
	var du2;
	var tc;
	var dl;
	var du;
	var df;
	var d;
	var b;
	var x;

	tc = n_zero;
	ferr = new Float64Array( 1 );
	berr = new Float64Array( 1 );
	work = new Float64Array( 1 );
	iwork = new Int32Array( 1 );
	dl = new Float64Array( 0 );
	d = new Float64Array( 0 );
	du = new Float64Array( 0 );
	dlf = new Float64Array( 0 );
	df = new Float64Array( 0 );
	duf = new Float64Array( 0 );
	du2 = new Float64Array( 0 );
	ipiv = new Int32Array( 0 );
	b = new Float64Array( 0 );
	x = new Float64Array( 0 );
	info = dgtrfs( 'no-transpose', 0, 1, dl, 1, 0, d, 1, 0, du, 1, 0, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, b, 1, 0, 0, x, 1, 0, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0, iwork, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dgtrfs: pivot_5x5_notrans', function t() {
	var iwork;
	var ipiv;
	var ferr;
	var berr;
	var work;
	var info;
	var dlf;
	var duf;
	var du2;
	var tc;
	var dl;
	var du;
	var df;
	var N;
	var d;
	var b;
	var x;

	tc = pivot_5x5_notrans;
	N = 5;
	dl = toF64( [ 5.0, 7.0, 9.0, 2.0 ] );
	d = toF64( [ 1.0, 3.0, 2.0, 1.0, 8.0 ] );
	du = toF64( [ 2.0, 4.0, 6.0, 3.0 ] );
	dlf = toF64( [ 5.0, 7.0, 9.0, 2.0 ] );
	df = toF64( [ 1.0, 3.0, 2.0, 1.0, 8.0 ] );
	duf = toF64( [ 2.0, 4.0, 6.0, 3.0 ] );
	du2 = new Float64Array( 3 );
	ipiv = new Int32Array( 5 );
	dgttrf( N, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0 );
	b = toF64( [ 3.0, 12.0, 15.0, 13.0, 10.0 ] );
	x = toF64( [ 3.0, 12.0, 15.0, 13.0, 10.0 ] );
	dgttrs( 'no-transpose', N, 1, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, x, 1, N, 0 ); // eslint-disable-line max-len
	ferr = new Float64Array( 1 );
	berr = new Float64Array( 1 );
	work = new Float64Array( 3 * N );
	iwork = new Int32Array( N );
	info = dgtrfs( 'no-transpose', N, 1, dl, 1, 0, d, 1, 0, du, 1, 0, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, b, 1, N, 0, x, 1, N, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0, iwork, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});
