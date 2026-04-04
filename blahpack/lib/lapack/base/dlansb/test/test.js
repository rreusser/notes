/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlansb = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlansb.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// VARIABLES //

// Band storage for upper symmetric 5x5 matrix with K=2 (LDAB=3), column-major layout
//
// Full symmetric matrix A:
//   [ 1  -4   7   0   0 ]
//   [-4   5  -8   6   0 ]
//   [ 7  -8   9  -3   2 ]
//   [ 0   6  -3   4  -1 ]
//   [ 0   0   2  -1   3 ]
var upperAB = new Float64Array([
	0,
	0,
	1,       // col 0
	0,
	-4,
	5,      // col 1
	7,
	-8,
	9,      // col 2
	6,
	-3,
	4,      // col 3
	2,
	-1,
	3       // col 4
]);

// Band storage for lower symmetric 5x5 matrix with K=2 (LDAB=3), column-major layout
//
// Full symmetric matrix A:
//   [ 2  -3   1   0   0 ]
//   [-3   6  -5   7   0 ]
//   [ 1  -5   8  -2  -4 ]
//   [ 0   7  -2   3   1 ]
//   [ 0   0  -4   1   5 ]
var lowerAB = new Float64Array([
	2,
	-3,
	1,      // col 0
	6,
	-5,
	7,      // col 1
	8,
	-2,
	-4,     // col 2
	3,
	1,
	0,       // col 3
	5,
	0,
	0        // col 4
]);

// K=0 diagonal matrix (LDAB=1), N=4: diag(3, -7, 2, -4)
var diagK0 = new Float64Array([ 3, -7, 2, -4 ]);

// K=1 upper symmetric 4x4, column-major layout (LDAB=2)
//
// Full symmetric matrix A:
//   [ 2  -3   0   0 ]
//   [-3   4   1   0 ]
//   [ 0   1  -5   6 ]
//   [ 0   0   6   7 ]
var upperK1 = new Float64Array([
	0,
	2,       // col 0
	-3,
	4,      // col 1
	1,
	-5,      // col 2
	6,
	7        // col 3
]);

// K=1 lower symmetric 4x4, column-major layout (LDAB=2)
//
// Full symmetric matrix A (same as upper K1):
//   [ 2  -3   0   0 ]
//   [-3   4   1   0 ]
//   [ 0   1  -5   6 ]
//   [ 0   0   6   7 ]
var lowerK1 = new Float64Array([
	2,
	-3,      // col 0
	4,
	1,       // col 1
	-5,
	6,      // col 2
	7,
	0        // col 3
]);

// 1x1 matrix, K=0
var oneByOne = new Float64Array([ 5.0 ]);

var work = new Float64Array( 10 );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	} );
}

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


// TESTS //

test( 'dlansb: upper_max', function t() {
	var result = dlansb( 'max', 'upper', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: upper_one', function t() {
	var result = dlansb( 'one-norm', 'upper', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: upper_inf', function t() {
	var result = dlansb( 'inf-norm', 'upper', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: upper_frob', function t() {
	var result = dlansb( 'frobenius', 'upper', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: lower_max', function t() {
	var result = dlansb( 'max', 'lower', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: lower_one', function t() {
	var result = dlansb( 'one-norm', 'lower', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: lower_inf', function t() {
	var result = dlansb( 'inf-norm', 'lower', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: lower_frob', function t() {
	var result = dlansb( 'frobenius', 'lower', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: edge_n0', function t() {
	var result = dlansb( 'max', 'upper', 0, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});

test( 'dlansb: edge_1x1_max', function t() {
	var result = dlansb( 'max', 'upper', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'edge_1x1_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: edge_1x1_one', function t() {
	var result = dlansb( 'one-norm', 'upper', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'edge_1x1_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: edge_1x1_inf', function t() {
	var result = dlansb( 'inf-norm', 'upper', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'edge_1x1_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: edge_1x1_frob', function t() {
	var result = dlansb( 'frobenius', 'upper', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'edge_1x1_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: returns 0 for unknown norm type', function t() {
	var result = dlansb( 'unknown', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});

test( 'dlansb: diag_k0_upper_max', function t() {
	var result = dlansb( 'max', 'upper', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'diag_k0_upper_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: diag_k0_upper_one', function t() {
	var result = dlansb( 'one-norm', 'upper', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'diag_k0_upper_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: diag_k0_upper_inf', function t() {
	var result = dlansb( 'inf-norm', 'upper', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'diag_k0_upper_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: diag_k0_upper_frob', function t() {
	var result = dlansb( 'frobenius', 'upper', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'diag_k0_upper_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: upper_k1_max', function t() {
	var result = dlansb( 'max', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: upper_k1_one', function t() {
	var result = dlansb( 'one-norm', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: upper_k1_inf', function t() {
	var result = dlansb( 'inf-norm', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: upper_k1_frob', function t() {
	var result = dlansb( 'frobenius', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: lower_k1_max', function t() {
	var result = dlansb( 'max', 'lower', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: lower_k1_one', function t() {
	var result = dlansb( 'one-norm', 'lower', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: lower_k1_inf', function t() {
	var result = dlansb( 'inf-norm', 'lower', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansb: lower_k1_frob', function t() {
	var result = dlansb( 'frobenius', 'lower', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});
