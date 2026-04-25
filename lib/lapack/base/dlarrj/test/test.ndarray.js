/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlarrj = require( './../lib/ndarray.js' );

// FIXTURES //

var diagonal_4x4 = require( './fixtures/diagonal_4x4.json' );
var tridiag_3x3 = require( './fixtures/tridiag_3x3.json' );
var n_one = require( './fixtures/n_one.json' );
var subset_refinement = require( './fixtures/subset_refinement.json' );
var with_offset = require( './fixtures/with_offset.json' );
var already_converged = require( './fixtures/already_converged.json' );
var tridiag_5x5 = require( './fixtures/tridiag_5x5.json' );
var coarse_rtol = require( './fixtures/coarse_rtol.json' );
// CONSTANTS //

var PIVMIN = 2.2250738585072014e-308; // Number.MIN_VALUE (DLAMCH('S'))

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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
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

test( 'dlarrj is a function', function t() {
	assert.equal( typeof dlarrj, 'function' );
});

test( 'dlarrj: diagonal_4x4', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = diagonal_4x4;
	w = new Float64Array( [ 1.1, 2.9, 5.2, 6.8 ] );
	WERR = new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] );
	WORK = new Float64Array( 16 );
	IWORK = new Int32Array( 16 );
	info = dlarrj(4, new Float64Array( [ 1.0, 3.0, 5.0, 7.0 ] ), 1, 0, new Float64Array( [ 0.0, 0.0, 0.0 ] ), 1, 0, 1, 4, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 6.0);
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( toArray( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: tridiag_3x3', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = tridiag_3x3;
	w = new Float64Array( [ 1.1, 2.1, 3.9 ] );
	WERR = new Float64Array( [ 0.5, 0.5, 0.5 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );
	info = dlarrj(3, new Float64Array( [ 2.0, 3.0, 2.0 ] ), 1, 0, new Float64Array( [ 1.0, 1.0 ] ), 1, 0, 1, 3, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 3.0);
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( toArray( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: n_zero', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var w;

	w = new Float64Array( 1 );
	WERR = new Float64Array( 1 );
	WORK = new Float64Array( 4 );
	IWORK = new Int32Array( 4 );
	info = dlarrj(0, new Float64Array( 1 ), 1, 0, new Float64Array( 1 ), 1, 0, 1, 0, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 1.0);
	assert.equal( info, 0 );
});

test( 'dlarrj: n_one', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = n_one;
	w = new Float64Array( [ 5.1 ] );
	WERR = new Float64Array( [ 0.5 ] );
	WORK = new Float64Array( 4 );
	IWORK = new Int32Array( 4 );
	info = dlarrj(1, new Float64Array( [ 5.0 ] ), 1, 0, new Float64Array( 1 ), 1, 0, 1, 1, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 1.0);
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( toArray( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: subset_refinement', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = subset_refinement;
	w = new Float64Array( [ 0.0, 3.1, 4.9, 0.0 ] );
	WERR = new Float64Array( [ 0.0, 0.5, 0.5, 0.0 ] );
	WORK = new Float64Array( 16 );
	IWORK = new Int32Array( 16 );
	info = dlarrj(4, new Float64Array( [ 1.0, 3.0, 5.0, 7.0 ] ), 1, 0, new Float64Array( [ 0.0, 0.0, 0.0 ] ), 1, 0, 2, 3, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 6.0);
	assert.equal( info, tc.info );
	assertArrayClose( [ w[ 1 ], w[ 2 ] ], tc.w, 1e-12, 'w' );
	assertArrayClose( [ WERR[ 1 ], WERR[ 2 ] ], tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: with_offset', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = with_offset;
	w = new Float64Array( [ 5.1, 6.9 ] );
	WERR = new Float64Array( [ 0.5, 0.5 ] );
	WORK = new Float64Array( 16 );
	IWORK = new Int32Array( 16 );
	info = dlarrj(4, new Float64Array( [ 1.0, 3.0, 5.0, 7.0 ] ), 1, 0, new Float64Array( [ 0.0, 0.0, 0.0 ] ), 1, 0, 3, 4, 1e-14, 2, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 6.0);
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( toArray( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: already_converged', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = already_converged;
	w = new Float64Array( [ 1.0, 2.0, 4.0 ] );
	WERR = new Float64Array( [ 1e-16, 1e-16, 1e-16 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );
	info = dlarrj(3, new Float64Array( [ 2.0, 3.0, 2.0 ] ), 1, 0, new Float64Array( [ 1.0, 1.0 ] ), 1, 0, 1, 3, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 3.0);
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( toArray( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: tridiag_5x5', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = tridiag_5x5;
	w = new Float64Array( [ 1.0, 2.5, 4.0, 5.5, 7.0 ] );
	WERR = new Float64Array( [ 1.0, 1.0, 1.0, 1.0, 1.0 ] );
	WORK = new Float64Array( 20 );
	IWORK = new Int32Array( 20 );
	info = dlarrj(5, new Float64Array( [ 4.0, 3.0, 2.0, 5.0, 6.0 ] ), 1, 0, new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] ), 1, 0, 1, 5, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 6.0);
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( toArray( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: left boundary expansion needed', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var w;

	w = new Float64Array( [ 1.5, 3.0, 5.0 ] );
	WERR = new Float64Array( [ 0.1, 0.5, 0.5 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );
	info = dlarrj(3, new Float64Array( [ 1.0, 3.0, 5.0 ] ), 1, 0, new Float64Array( [ 0.0, 0.0 ] ), 1, 0, 1, 3, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 4.0);
	assert.equal( info, 0 );
	assertClose( w[ 0 ], 1.0, 1e-10, 'w[0]' );
	assertClose( w[ 1 ], 3.0, 1e-10, 'w[1]' );
	assertClose( w[ 2 ], 5.0, 1e-10, 'w[2]' );
});

test( 'dlarrj: right boundary expansion needed', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var w;

	w = new Float64Array( [ 1.0, 3.0, 4.5 ] );
	WERR = new Float64Array( [ 0.5, 0.5, 0.1 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );
	info = dlarrj(3, new Float64Array( [ 1.0, 3.0, 5.0 ] ), 1, 0, new Float64Array( [ 0.0, 0.0 ] ), 1, 0, 1, 3, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 4.0);
	assert.equal( info, 0 );
	assertClose( w[ 0 ], 1.0, 1e-10, 'w[0]' );
	assertClose( w[ 1 ], 3.0, 1e-10, 'w[1]' );
	assertClose( w[ 2 ], 5.0, 1e-10, 'w[2]' );
});

test( 'dlarrj: mixed converged and unconverged', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var w;

	w = new Float64Array( [ 1.0, 3.1, 5.0, 6.8 ] );
	WERR = new Float64Array( [ 1e-16, 0.5, 1e-16, 0.5 ] );
	WORK = new Float64Array( 16 );
	IWORK = new Int32Array( 16 );
	info = dlarrj(4, new Float64Array( [ 1.0, 3.0, 5.0, 7.0 ] ), 1, 0, new Float64Array( [ 0.0, 0.0, 0.0 ] ), 1, 0, 1, 4, 1e-14, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 6.0);
	assert.equal( info, 0 );
	assertClose( w[ 0 ], 1.0, 1e-12, 'w[0] (already converged)' );
	assertClose( w[ 1 ], 3.0, 1e-10, 'w[1] (refined)' );
	assertClose( w[ 2 ], 5.0, 1e-12, 'w[2] (already converged)' );
	assertClose( w[ 3 ], 7.0, 1e-10, 'w[3] (refined)' );
});

test( 'dlarrj: coarse_rtol', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = coarse_rtol;
	w = new Float64Array( [ 1.1, 2.1, 3.9 ] );
	WERR = new Float64Array( [ 0.5, 0.5, 0.5 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );
	info = dlarrj(3, new Float64Array( [ 2.0, 3.0, 2.0 ] ), 1, 0, new Float64Array( [ 1.0, 1.0 ] ), 1, 0, 1, 3, 1e-4, 0, w, 1, 0, WERR, 1, 0, WORK, 1, 0, IWORK, 1, 0, PIVMIN, 3.0);
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( toArray( WERR ), tc.werr, 1e-6, 'werr' );
});
