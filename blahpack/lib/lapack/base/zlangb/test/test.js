/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlangb = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zlangb.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


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

/**
* Creates the 5x5 general band matrix with KL=1, KU=2 (LDAB=4, column-major).
*
* Full matrix A:
*   A = [ (1+2i)  (3+4i)  (5+6i)    0       0   ]
*       [ (7+8i)  (9+1i)  (2+3i)  (4+5i)    0   ]
*       [    0    (6+7i)  (8+9i)  (1+2i)  (3+4i) ]
*       [    0       0    (5+6i)  (7+8i)  (9+1i) ]
*       [    0       0       0    (2+3i)  (4+5i) ]
*
* Band storage (LDAB=4):
*   Row 0 (2nd superdiag):  .       .      (5+6i)  (4+5i)  (3+4i)
*   Row 1 (1st superdiag):  .      (3+4i)  (2+3i)  (1+2i)  (9+1i)
*   Row 2 (diagonal):      (1+2i)  (9+1i)  (8+9i)  (7+8i)  (4+5i)
*   Row 3 (1st subdiag):   (7+8i)  (6+7i)  (5+6i)  (2+3i)     .
*
* @private
* @returns {Complex128Array} band matrix
*/
function make5x5AB( ) {
	return new Complex128Array([
		// Col 0: rows 0,1,2,3
		0,
		0,
		0,
		0,
		1,
		2,
		7,
		8,

		// Col 1: rows 0,1,2,3
		0,
		0,
		3,
		4,
		9,
		1,
		6,
		7,

		// Col 2: rows 0,1,2,3
		5,
		6,
		2,
		3,
		8,
		9,
		5,
		6,

		// Col 3: rows 0,1,2,3
		4,
		5,
		1,
		2,
		7,
		8,
		2,
		3,

		// Col 4: rows 0,1,2,3
		3,
		4,
		9,
		1,
		4,
		5,
		0,
		0
	]);
}

/**
* Creates the 4x4 tridiagonal band matrix KL=1, KU=1 (LDAB=3, column-major).
*
* Full matrix A:
*   A = [ (1+1i)  (2+2i)    0       0   ]
*       [ (3+3i)  (4+4i)  (5+5i)    0   ]
*       [    0    (6+6i)  (7+7i)  (8+8i) ]
*       [    0       0    (9+9i)  (1+1i) ]
*
* Band storage (LDAB=3):
*   Row 0 (superdiag):  .      (2+2i)  (5+5i)  (8+8i)
*   Row 1 (diagonal):  (1+1i)  (4+4i)  (7+7i)  (1+1i)
*   Row 2 (subdiag):   (3+3i)  (6+6i)  (9+9i)     .
*
* @private
* @returns {Complex128Array} band matrix
*/
function make4x4AB( ) {
	return new Complex128Array([
		// Col 0: rows 0,1,2
		0,
		0,
		1,
		1,
		3,
		3,

		// Col 1: rows 0,1,2
		2,
		2,
		4,
		4,
		6,
		6,

		// Col 2: rows 0,1,2
		5,
		5,
		7,
		7,
		9,
		9,

		// Col 3: rows 0,1,2
		8,
		8,
		1,
		1,
		0,
		0
	]);
}


// TESTS //

// --- 5x5 general band, KL=1, KU=2 ---

test( 'zlangb: max_5x5', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'max_5x5' );
	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = zlangb( 'max', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: one_5x5', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'one_5x5' );
	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = zlangb( 'one-norm', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: inf_5x5', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'inf_5x5' );
	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = zlangb( 'inf-norm', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: frob_5x5', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'frob_5x5' );
	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = zlangb( 'frobenius', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- 4x4 tridiagonal, KL=1, KU=1 ---

test( 'zlangb: max_4x4_tridiag', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'max_4x4_tridiag' );
	AB = make4x4AB();
	WORK = new Float64Array( 10 );
	result = zlangb( 'max', 4, 1, 1, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: one_4x4_tridiag', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'one_4x4_tridiag' );
	AB = make4x4AB();
	WORK = new Float64Array( 10 );
	result = zlangb( 'one-norm', 4, 1, 1, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: inf_4x4_tridiag', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'inf_4x4_tridiag' );
	AB = make4x4AB();
	WORK = new Float64Array( 10 );
	result = zlangb( 'inf-norm', 4, 1, 1, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: frob_4x4_tridiag', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'frob_4x4_tridiag' );
	AB = make4x4AB();
	WORK = new Float64Array( 10 );
	result = zlangb( 'frobenius', 4, 1, 1, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Edge cases ---

test( 'zlangb: n_zero', function t() {
	var result;
	var WORK;
	var AB;

	AB = new Complex128Array( 1 );
	WORK = new Float64Array( 1 );
	result = zlangb( 'max', 0, 1, 2, AB, 1, 1, 0, WORK, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'zlangb: max_diag_only (KL=0, KU=0)', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'max_diag_only' );
	AB = new Complex128Array([
		3, 4, 1, 1, 2, 2
	]);
	WORK = new Float64Array( 10 );
	result = zlangb( 'max', 3, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: one_diag_only (KL=0, KU=0)', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'one_diag_only' );
	AB = new Complex128Array([
		3, 4, 1, 1, 2, 2
	]);
	WORK = new Float64Array( 10 );
	result = zlangb( 'one-norm', 3, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: inf_diag_only (KL=0, KU=0)', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'inf_diag_only' );
	AB = new Complex128Array([
		3, 4, 1, 1, 2, 2
	]);
	WORK = new Float64Array( 10 );
	result = zlangb( 'inf-norm', 3, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: frob_diag_only (KL=0, KU=0)', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'frob_diag_only' );
	AB = new Complex128Array([
		3, 4, 1, 1, 2, 2
	]);
	WORK = new Float64Array( 10 );
	result = zlangb( 'frobenius', 3, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: frob_1x1', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'frob_1x1' );
	AB = new Complex128Array( [ 3, 4 ] );
	WORK = new Float64Array( 1 );
	result = zlangb( 'frobenius', 1, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangb: one_1x1', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = findCase( 'one_1x1' );
	AB = new Complex128Array( [ 3, 4 ] );
	WORK = new Float64Array( 1 );
	result = zlangb( 'one-norm', 1, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Unknown norm returns 0 ---

test( 'zlangb: unknown norm returns 0', function t() {
	var result;
	var WORK;
	var AB;

	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = zlangb( 'unknown', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assert.equal( result, 0.0 );
});
