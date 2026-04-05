/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlansy = require( './../lib/base.js' );

// FIXTURES //

var dlansy_max_u = require( './fixtures/dlansy_max_u.json' );
var dlansy_one_u = require( './fixtures/dlansy_one_u.json' );
var dlansy_one_o_u = require( './fixtures/dlansy_one_o_u.json' );
var dlansy_inf_u = require( './fixtures/dlansy_inf_u.json' );
var dlansy_frob_u = require( './fixtures/dlansy_frob_u.json' );
var dlansy_frob_e_u = require( './fixtures/dlansy_frob_e_u.json' );
var dlansy_max_l = require( './fixtures/dlansy_max_l.json' );
var dlansy_one_l = require( './fixtures/dlansy_one_l.json' );
var dlansy_inf_l = require( './fixtures/dlansy_inf_l.json' );
var dlansy_frob_l = require( './fixtures/dlansy_frob_l.json' );
var dlansy_n_zero = require( './fixtures/dlansy_n_zero.json' );
var dlansy_1x1_max = require( './fixtures/dlansy_1x1_max.json' );
var dlansy_1x1_one = require( './fixtures/dlansy_1x1_one.json' );
var dlansy_1x1_inf = require( './fixtures/dlansy_1x1_inf.json' );
var dlansy_1x1_frob = require( './fixtures/dlansy_1x1_frob.json' );
var dlansy_5x5_max_u = require( './fixtures/dlansy_5x5_max_u.json' );
var dlansy_5x5_one_u = require( './fixtures/dlansy_5x5_one_u.json' );
var dlansy_5x5_inf_u = require( './fixtures/dlansy_5x5_inf_u.json' );
var dlansy_5x5_frob_u = require( './fixtures/dlansy_5x5_frob_u.json' );
var dlansy_5x5_max_l = require( './fixtures/dlansy_5x5_max_l.json' );
var dlansy_5x5_one_l = require( './fixtures/dlansy_5x5_one_l.json' );
var dlansy_5x5_inf_l = require( './fixtures/dlansy_5x5_inf_l.json' );
var dlansy_5x5_frob_l = require( './fixtures/dlansy_5x5_frob_l.json' );

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
* Creates a column-major matrix from element assignments.
*
* @private
* @param {NonNegativeInteger} LDA - leading dimension (number of rows in storage)
* @param {NonNegativeInteger} N - number of columns
* @param {Object} entries - map from "i,j" (0-based) to value
* @returns {Float64Array} column-major matrix
*/
function makeMatrix( LDA, N, entries ) {
	var parts;
	var keys = Object.keys( entries );
	var A = new Float64Array( LDA * N );
	var i;
	var j;
	var k;
	for ( k = 0; k < keys.length; k++ ) {
		parts = keys[ k ].split( ',' );
		i = parseInt( parts[ 0 ], 10 );
		j = parseInt( parts[ 1 ], 10 );
		A[ j * LDA + i ] = entries[ keys[ k ] ];
	}
	return A;
}

// TESTS //

// 4x4 symmetric matrix (upper triangle stored):
// A = [  2.0   3.0  -1.0   4.0 ]
//     [  3.0   5.0   2.0  -6.0 ]
//     [ -1.0   2.0   7.0   1.0 ]
//     [  4.0  -6.0   1.0   8.0 ]

var A4_UPPER = makeMatrix( 4, 4, {
	'0,0': 2.0,
	'0,1': 3.0,
	'1,1': 5.0,
	'0,2': -1.0,
	'1,2': 2.0,
	'2,2': 7.0,
	'0,3': 4.0,
	'1,3': -6.0,
	'2,3': 1.0,
	'3,3': 8.0
});

var A4_LOWER = makeMatrix( 4, 4, {
	'0,0': 2.0,
	'1,0': 3.0,
	'1,1': 5.0,
	'2,0': -1.0,
	'2,1': 2.0,
	'2,2': 7.0,
	'3,0': 4.0,
	'3,1': -6.0,
	'3,2': 1.0,
	'3,3': 8.0
});

// 5x5 symmetric matrix
var A5_UPPER = makeMatrix( 5, 5, {
	'0,0': 1.0,
	'0,1': 2.0,
	'1,1': 3.0,
	'0,2': 4.0,
	'1,2': 5.0,
	'2,2': 6.0,
	'0,3': 7.0,
	'1,3': 8.0,
	'2,3': 9.0,
	'3,3': 10.0,
	'0,4': 11.0,
	'1,4': 12.0,
	'2,4': 13.0,
	'3,4': 14.0,
	'4,4': 15.0
});

var A5_LOWER = makeMatrix( 5, 5, {
	'0,0': 1.0,
	'1,0': 2.0,
	'1,1': 3.0,
	'2,0': 4.0,
	'2,1': 5.0,
	'2,2': 6.0,
	'3,0': 7.0,
	'3,1': 8.0,
	'3,2': 9.0,
	'3,3': 10.0,
	'4,0': 11.0,
	'4,1': 12.0,
	'4,2': 13.0,
	'4,3': 14.0,
	'4,4': 15.0
});

test( 'dlansy: dlansy_max_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_max_u;
	WORK = new Float64Array( 4 );
	result = dlansy( 'max', 'upper', 4, A4_UPPER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_one_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_one_u;
	WORK = new Float64Array( 4 );
	result = dlansy( 'one-norm', 'upper', 4, A4_UPPER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_one_O_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_one_o_u;
	WORK = new Float64Array( 4 );
	result = dlansy( 'one-norm', 'upper', 4, A4_UPPER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_inf_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_inf_u;
	WORK = new Float64Array( 4 );
	result = dlansy( 'inf-norm', 'upper', 4, A4_UPPER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_frob_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_frob_u;
	WORK = new Float64Array( 4 );
	result = dlansy( 'frobenius', 'upper', 4, A4_UPPER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_frob_E_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_frob_e_u;
	WORK = new Float64Array( 4 );
	result = dlansy( 'frobenius', 'upper', 4, A4_UPPER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_max_L', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_max_l;
	WORK = new Float64Array( 4 );
	result = dlansy( 'max', 'lower', 4, A4_LOWER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_one_L', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_one_l;
	WORK = new Float64Array( 4 );
	result = dlansy( 'one-norm', 'lower', 4, A4_LOWER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_inf_L', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_inf_l;
	WORK = new Float64Array( 4 );
	result = dlansy( 'inf-norm', 'lower', 4, A4_LOWER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_frob_L', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_frob_l;
	WORK = new Float64Array( 4 );
	result = dlansy( 'frobenius', 'lower', 4, A4_LOWER, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_n_zero', function t() {
	var result;
	var WORK;
	var tc;
	var A;

	tc = dlansy_n_zero;
	WORK = new Float64Array( 4 );
	A = new Float64Array( 1 );
	result = dlansy( 'max', 'upper', 0, A, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_1x1_max', function t() {
	var result;
	var WORK;
	var tc;
	var A;

	tc = dlansy_1x1_max;
	WORK = new Float64Array( 1 );
	A = new Float64Array( [ -5.5 ] );
	result = dlansy( 'max', 'upper', 1, A, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_1x1_one', function t() {
	var result;
	var WORK;
	var tc;
	var A;

	tc = dlansy_1x1_one;
	WORK = new Float64Array( 1 );
	A = new Float64Array( [ -5.5 ] );
	result = dlansy( 'one-norm', 'upper', 1, A, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_1x1_inf', function t() {
	var result;
	var WORK;
	var tc;
	var A;

	tc = dlansy_1x1_inf;
	WORK = new Float64Array( 1 );
	A = new Float64Array( [ -5.5 ] );
	result = dlansy( 'inf-norm', 'upper', 1, A, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_1x1_frob', function t() {
	var result;
	var WORK;
	var tc;
	var A;

	tc = dlansy_1x1_frob;
	WORK = new Float64Array( 1 );
	A = new Float64Array( [ -5.5 ] );
	result = dlansy( 'frobenius', 'upper', 1, A, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_5x5_max_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_5x5_max_u;
	WORK = new Float64Array( 5 );
	result = dlansy( 'max', 'upper', 5, A5_UPPER, 1, 5, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_5x5_one_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_5x5_one_u;
	WORK = new Float64Array( 5 );
	result = dlansy( 'one-norm', 'upper', 5, A5_UPPER, 1, 5, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_5x5_inf_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_5x5_inf_u;
	WORK = new Float64Array( 5 );
	result = dlansy( 'inf-norm', 'upper', 5, A5_UPPER, 1, 5, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_5x5_frob_U', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_5x5_frob_u;
	WORK = new Float64Array( 5 );
	result = dlansy( 'frobenius', 'upper', 5, A5_UPPER, 1, 5, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_5x5_max_L', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_5x5_max_l;
	WORK = new Float64Array( 5 );
	result = dlansy( 'max', 'lower', 5, A5_LOWER, 1, 5, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_5x5_one_L', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_5x5_one_l;
	WORK = new Float64Array( 5 );
	result = dlansy( 'one-norm', 'lower', 5, A5_LOWER, 1, 5, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_5x5_inf_L', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_5x5_inf_l;
	WORK = new Float64Array( 5 );
	result = dlansy( 'inf-norm', 'lower', 5, A5_LOWER, 1, 5, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansy: dlansy_5x5_frob_L', function t() {
	var result;
	var WORK;
	var tc;

	tc = dlansy_5x5_frob_l;
	WORK = new Float64Array( 5 );
	result = dlansy( 'frobenius', 'lower', 5, A5_LOWER, 1, 5, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
