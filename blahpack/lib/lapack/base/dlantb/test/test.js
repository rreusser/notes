/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlantb = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlantb.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// VARIABLES //

// Band storage for upper triangular 5x5 matrix with K=2 (LDAB=3), column-major layout
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

// Band storage for lower triangular 5x5 matrix with K=2 (LDAB=3), column-major layout
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

// K=1 upper triangular 4x4, column-major layout (LDAB=2)
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

// K=1 lower triangular 4x4, column-major layout (LDAB=2)
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

test( 'dlantb: upper_nonunit_max', function t() {
	var result = dlantb( 'max', 'upper', 'non-unit', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_nonunit_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_nonunit_one', function t() {
	var result = dlantb( 'one-norm', 'upper', 'non-unit', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_nonunit_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_nonunit_inf', function t() {
	var result = dlantb( 'inf-norm', 'upper', 'non-unit', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_nonunit_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_nonunit_frob', function t() {
	var result = dlantb( 'frobenius', 'upper', 'non-unit', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_nonunit_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_unit_max', function t() {
	var result = dlantb( 'max', 'upper', 'unit', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_unit_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_unit_one', function t() {
	var result = dlantb( 'one-norm', 'upper', 'unit', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_unit_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_unit_inf', function t() {
	var result = dlantb( 'inf-norm', 'upper', 'unit', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_unit_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_unit_frob', function t() {
	var result = dlantb( 'frobenius', 'upper', 'unit', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_unit_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_nonunit_max', function t() {
	var result = dlantb( 'max', 'lower', 'non-unit', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_nonunit_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_nonunit_one', function t() {
	var result = dlantb( 'one-norm', 'lower', 'non-unit', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_nonunit_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_nonunit_inf', function t() {
	var result = dlantb( 'inf-norm', 'lower', 'non-unit', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_nonunit_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_nonunit_frob', function t() {
	var result = dlantb( 'frobenius', 'lower', 'non-unit', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_nonunit_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_unit_max', function t() {
	var result = dlantb( 'max', 'lower', 'unit', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_unit_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_unit_one', function t() {
	var result = dlantb( 'one-norm', 'lower', 'unit', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_unit_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_unit_inf', function t() {
	var result = dlantb( 'inf-norm', 'lower', 'unit', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_unit_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_unit_frob', function t() {
	var result = dlantb( 'frobenius', 'lower', 'unit', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_unit_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: edge_n0', function t() {
	var result = dlantb( 'max', 'upper', 'non-unit', 0, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});

test( 'dlantb: edge_1x1_max', function t() {
	var result = dlantb( 'max', 'upper', 'non-unit', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'edge_1x1_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: edge_1x1_one', function t() {
	var result = dlantb( 'one-norm', 'upper', 'non-unit', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'edge_1x1_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: edge_1x1_inf', function t() {
	var result = dlantb( 'inf-norm', 'upper', 'non-unit', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'edge_1x1_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: edge_1x1_frob', function t() {
	var result = dlantb( 'frobenius', 'upper', 'non-unit', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'edge_1x1_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: edge_1x1_unit_max', function t() {
	var result = dlantb( 'max', 'upper', 'unit', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 1.0 );
});

test( 'dlantb: returns 0 for unknown norm type', function t() {
	var result = dlantb( 'unknown', 'upper', 'non-unit', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});

test( 'dlantb: diag_k0_upper_max', function t() {
	var result = dlantb( 'max', 'upper', 'non-unit', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'diag_k0_upper_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: diag_k0_upper_one', function t() {
	var result = dlantb( 'one-norm', 'upper', 'non-unit', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'diag_k0_upper_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: diag_k0_upper_inf', function t() {
	var result = dlantb( 'inf-norm', 'upper', 'non-unit', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'diag_k0_upper_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: diag_k0_upper_frob', function t() {
	var result = dlantb( 'frobenius', 'upper', 'non-unit', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'diag_k0_upper_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_k1_nonunit_max', function t() {
	var result = dlantb( 'max', 'upper', 'non-unit', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_nonunit_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_k1_nonunit_one', function t() {
	var result = dlantb( 'one-norm', 'upper', 'non-unit', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_nonunit_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_k1_nonunit_inf', function t() {
	var result = dlantb( 'inf-norm', 'upper', 'non-unit', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_nonunit_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_k1_nonunit_frob', function t() {
	var result = dlantb( 'frobenius', 'upper', 'non-unit', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_nonunit_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_k1_unit_max', function t() {
	var result = dlantb( 'max', 'upper', 'unit', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_unit_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_k1_unit_one', function t() {
	var result = dlantb( 'one-norm', 'upper', 'unit', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_unit_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_k1_unit_inf', function t() {
	var result = dlantb( 'inf-norm', 'upper', 'unit', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_unit_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: upper_k1_unit_frob', function t() {
	var result = dlantb( 'frobenius', 'upper', 'unit', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'upper_k1_unit_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_k1_nonunit_max', function t() {
	var result = dlantb( 'max', 'lower', 'non-unit', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_nonunit_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_k1_nonunit_one', function t() {
	var result = dlantb( 'one-norm', 'lower', 'non-unit', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_nonunit_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_k1_nonunit_inf', function t() {
	var result = dlantb( 'inf-norm', 'lower', 'non-unit', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_nonunit_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_k1_nonunit_frob', function t() {
	var result = dlantb( 'frobenius', 'lower', 'non-unit', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_nonunit_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_k1_unit_max', function t() {
	var result = dlantb( 'max', 'lower', 'unit', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_unit_max' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_k1_unit_one', function t() {
	var result = dlantb( 'one-norm', 'lower', 'unit', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_unit_one' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_k1_unit_inf', function t() {
	var result = dlantb( 'inf-norm', 'lower', 'unit', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_unit_inf' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantb: lower_k1_unit_frob', function t() {
	var result = dlantb( 'frobenius', 'lower', 'unit', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = findCase( 'lower_k1_unit_frob' );
	assertClose( result, tc.result, 1e-14, 'result' );
});
