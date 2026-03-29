/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlangt = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlangt.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	return fixture.find( function find( t ) { return t.name === name;
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

test( 'dlangt: max_norm_4x4', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'max_norm_4x4' );
	dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	result = dlangt( 'max-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_4x4', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_4x4' );
	dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	result = dlangt( 'one-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_O alias same as one_norm', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_O_4x4' );
	dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	result = dlangt( 'one-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: inf_norm_4x4', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'inf_norm_4x4' );
	dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	result = dlangt( 'infinity-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_4x4', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_4x4' );
	dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	result = dlangt( 'frobenius-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_E alias same as frobenius-norm', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_E_4x4' );
	dl = new Float64Array( [ 3.0, 1.0, 2.0 ] );
	d = new Float64Array( [ 2.0, 4.0, 5.0, 6.0 ] );
	du = new Float64Array( [ -1.0, -2.0, -3.0 ] );
	result = dlangt( 'frobenius-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: max_norm_n1', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'max_norm_n1' );
	dl = new Float64Array( 0 );
	d = new Float64Array( [ -7.0 ] );
	du = new Float64Array( 0 );
	result = dlangt( 'max-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_n1', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_n1' );
	dl = new Float64Array( 0 );
	d = new Float64Array( [ -7.0 ] );
	du = new Float64Array( 0 );
	result = dlangt( 'one-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: inf_norm_n1', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'inf_norm_n1' );
	dl = new Float64Array( 0 );
	d = new Float64Array( [ -7.0 ] );
	du = new Float64Array( 0 );
	result = dlangt( 'infinity-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_n1', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_n1' );
	dl = new Float64Array( 0 );
	d = new Float64Array( [ -7.0 ] );
	du = new Float64Array( 0 );
	result = dlangt( 'frobenius-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: n_zero', function t() {
	var result;
	var dl;
	var du;
	var d;

	dl = new Float64Array( 0 );
	d = new Float64Array( 0 );
	du = new Float64Array( 0 );
	result = dlangt( 'max-norm', 0, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'dlangt: max_norm_5x5', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'max_norm_5x5' );
	dl = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	d = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	du = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	result = dlangt( 'max-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_5x5', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_5x5' );
	dl = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	d = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	du = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	result = dlangt( 'one-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: inf_norm_5x5', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'inf_norm_5x5' );
	dl = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	d = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	du = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	result = dlangt( 'infinity-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_5x5', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_5x5' );
	dl = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	d = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	du = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	result = dlangt( 'frobenius-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: max_norm_n2', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'max_norm_n2' );
	dl = new Float64Array( [ 0.5 ] );
	d = new Float64Array( [ 3.0, 4.0 ] );
	du = new Float64Array( [ 1.5 ] );
	result = dlangt( 'max-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: one_norm_n2', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_n2' );
	dl = new Float64Array( [ 0.5 ] );
	d = new Float64Array( [ 3.0, 4.0 ] );
	du = new Float64Array( [ 1.5 ] );
	result = dlangt( 'one-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: inf_norm_n2', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'inf_norm_n2' );
	dl = new Float64Array( [ 0.5 ] );
	d = new Float64Array( [ 3.0, 4.0 ] );
	du = new Float64Array( [ 1.5 ] );
	result = dlangt( 'infinity-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangt: frob_norm_n2', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_n2' );
	dl = new Float64Array( [ 0.5 ] );
	d = new Float64Array( [ 3.0, 4.0 ] );
	du = new Float64Array( [ 1.5 ] );
	result = dlangt( 'frobenius-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
