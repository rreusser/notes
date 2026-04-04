/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqge = require( './../lib/base.js' );

// FIXTURES //

var no_equil = require( './fixtures/no_equil.json' );
var row_equil = require( './fixtures/row_equil.json' );
var col_equil = require( './fixtures/col_equil.json' );
var both_equil = require( './fixtures/both_equil.json' );
var amax_large = require( './fixtures/amax_large.json' );
var amax_small = require( './fixtures/amax_small.json' );

// FUNCTIONS //

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
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}

// Common input matrix (3x3, column-major)
/**
* InputA.
*
* @private
* @returns {*} result
*/
function inputA( ) {
	return new Float64Array( [ 2.0, 1.0, 0.5, 1.0, 3.0, 1.0, 0.5, 1.0, 4.0 ] );
}

/**
* InputR.
*
* @private
* @returns {*} result
*/
function inputR( ) {
	return new Float64Array( [ 0.5, 1.0, 0.8 ] );
}

/**
* InputC.
*
* @private
* @returns {*} result
*/
function inputC( ) {
	return new Float64Array( [ 0.6, 1.0, 0.7 ] );
}

// TESTS //

test( 'dlaqge: no equilibration (rowcnd >= thresh, colcnd >= thresh, amax in range)', function t() { // eslint-disable-line max-len
	var equed;
	var tc;
	var A;

	tc = no_equil;
	A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.5, 0.6, 4.0 ); // eslint-disable-line max-len
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: row equilibration only (rowcnd < thresh)', function t() {
	var equed;
	var tc;
	var A;

	tc = row_equil;
	A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.01, 0.6, 4.0 ); // eslint-disable-line max-len
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: column equilibration only (colcnd < thresh)', function t() {
	var equed;
	var tc;
	var A;

	tc = col_equil;
	A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.5, 0.01, 4.0 ); // eslint-disable-line max-len
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: both row and column equilibration', function t() {
	var equed;
	var tc;
	var A;

	tc = both_equil;
	A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.01, 0.01, 4.0 ); // eslint-disable-line max-len
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: row scaling triggered by amax > large', function t() {
	var equed;
	var tc;
	var A;

	tc = amax_large;
	A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.5, 0.6, 1.0e300 ); // eslint-disable-line max-len
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: row scaling triggered by amax < small', function t() {
	var equed;
	var tc;
	var A;

	tc = amax_small;
	A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.5, 0.6, 1.0e-320 ); // eslint-disable-line max-len
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: quick return M=0', function t() {
	var equed = dlaqge( 0, 3, new Float64Array( 0 ), 1, 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 3 ), 1, 0, 0.5, 0.6, 4.0 ); // eslint-disable-line max-len
	assert.equal( equed, 'none' );
});

test( 'dlaqge: quick return N=0', function t() {
	var equed = dlaqge( 3, 0, new Float64Array( 0 ), 1, 1, 0, new Float64Array( 3 ), 1, 0, new Float64Array( 0 ), 1, 0, 0.5, 0.6, 4.0 ); // eslint-disable-line max-len
	assert.equal( equed, 'none' );
});
