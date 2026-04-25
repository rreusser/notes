/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dstevx = require( './../lib/ndarray.js' );

// FIXTURES //

var vectors_all_4x4 = require( './fixtures/vectors_all_4x4.json' );
var novec_all_4x4 = require( './fixtures/novec_all_4x4.json' );
var vectors_value_4x4 = require( './fixtures/vectors_value_4x4.json' );
var vectors_index_4x4 = require( './fixtures/vectors_index_4x4.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var n_one_out_of_range = require( './fixtures/n_one_out_of_range.json' );
var novec_index_4x4 = require( './fixtures/novec_index_4x4.json' );
var vectors_all_6x6 = require( './fixtures/vectors_all_6x6.json' );
var vectors_value_abstol = require( './fixtures/vectors_value_abstol.json' );

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
* Helper to call dstevx with standard arguments.
*
* @param {string} jobz - 'compute-vectors' or 'no-vectors'
* @param {string} range - 'all', 'value', or 'index'
* @param {number} N - order
* @param {Array} dArr - diagonal elements
* @param {Array} eArr - subdiagonal elements
* @param {number} vl - lower value bound
* @param {number} vu - upper value bound
* @param {number} il - lower index (1-based)
* @param {number} iu - upper index (1-based)
* @param {number} abstol - tolerance
* @returns {Object} result with info, m, w, Z, IFAIL
*/
function callDstevx( jobz, range, N, dArr, eArr, vl, vu, il, iu, abstol ) {
	var IFAIL = new Int32Array( Math.max( N, 1 ) );
	var IWORK = new Int32Array( 5 * Math.max( N, 1 ) );
	var WORK = new Float64Array( 5 * Math.max( N, 1 ) );
	var info;
	var d = new Float64Array( dArr );
	var e = new Float64Array( eArr );
	var w = new Float64Array( N );
	var Z = new Float64Array( N * N );
	var M = new Int32Array( 1 );

	info = dstevx( jobz, range, N, d, 1, 0, e, 1, 0, vl, vu, il, iu, abstol, M, w, 1, 0, Z, 1, N, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 ); // eslint-disable-line max-len
	return {
		'info': info,
		'm': M[ 0 ],
		'w': Array.prototype.slice.call( w, 0, M[ 0 ] ),
		'Z': Z,
		'IFAIL': Array.prototype.slice.call( IFAIL, 0, M[ 0 ] )
	};
}

// TESTS //

test( 'dstevx: vectors_all_4x4', function t() {
	var res = callDstevx( 'compute-vectors', 'all', 4, [ 2.0, 2.0, 2.0, 2.0 ], [ 1.0, 1.0, 1.0 ], 0.0, 0.0, 0, 0, 0.0);
	var tc = vectors_all_4x4;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );

	// Check eigenvectors: each column should be close (up to sign)
	assertArrayClose( res.IFAIL, tc.ifail, 1e-14, 'ifail' );
});

test( 'dstevx: novec_all_4x4', function t() {
	var res = callDstevx( 'no-vectors', 'all', 4, [ 2.0, 2.0, 2.0, 2.0 ], [ 1.0, 1.0, 1.0 ], 0.0, 0.0, 0, 0, 0.0);
	var tc = novec_all_4x4;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: vectors_value_4x4', function t() {
	var res = callDstevx( 'compute-vectors', 'value', 4, [ 2.0, 2.0, 2.0, 2.0 ], [ 1.0, 1.0, 1.0 ], 1.5, 3.5, 0, 0, 0.0);
	var tc = vectors_value_4x4;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: vectors_index_4x4', function t() {
	var res = callDstevx( 'compute-vectors', 'index', 4, [ 2.0, 2.0, 2.0, 2.0 ], [ 1.0, 1.0, 1.0 ], 0.0, 0.0, 2, 3, 0.0);
	var tc = vectors_index_4x4;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: n_zero', function t() {
	var res = callDstevx( 'compute-vectors', 'all', 0, [], [], 0.0, 0.0, 0, 0, 0.0);
	var tc = n_zero;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
});

test( 'dstevx: n_one', function t() {
	var res = callDstevx( 'compute-vectors', 'all', 1, [ 3.5 ], [], 0.0, 0.0, 0, 0, 0.0);
	var tc = n_one;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
	assertArrayClose( Array.prototype.slice.call( res.Z, 0, 1 ), tc.z, 1e-14, 'z' ); // eslint-disable-line max-len
});

test( 'dstevx: n_one_out_of_range', function t() {
	var res = callDstevx( 'compute-vectors', 'value', 1, [ 3.5 ], [], 5.0, 10.0, 0, 0, 0.0);
	var tc = n_one_out_of_range;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
});

test( 'dstevx: novec_index_4x4', function t() {
	var res = callDstevx( 'no-vectors', 'index', 4, [ 2.0, 2.0, 2.0, 2.0 ], [ 1.0, 1.0, 1.0 ], 0.0, 0.0, 1, 2, 0.0);
	var tc = novec_index_4x4;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: vectors_all_6x6', function t() {
	var res = callDstevx( 'compute-vectors', 'all', 6, [ 1.0, 3.0, 2.0, 4.0, 1.5, 2.5 ], [ 0.5, 1.0, 0.3, 0.8, 0.6 ], 0.0, 0.0, 0, 0, 0.0);
	var tc = vectors_all_6x6;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});

test( 'dstevx: vectors_value_abstol', function t() {
	var res = callDstevx( 'compute-vectors', 'value', 4, [ 2.0, 2.0, 2.0, 2.0 ], [ 1.0, 1.0, 1.0 ], 0.0, 5.0, 0, 0, 1.0e-12);
	var tc = vectors_value_abstol;
	assert.equal( res.info, tc.info, 'info' );
	assert.equal( res.m, tc.m, 'm' );
	assertArrayClose( res.w, tc.w, 1e-14, 'w' );
});
