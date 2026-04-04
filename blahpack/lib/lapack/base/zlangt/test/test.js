/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zlangt = require( './../lib' );
var base = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zlangt.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'zlangt: main export is a function', function t() {
	assert.strictEqual( typeof zlangt, 'function' );
});

test( 'zlangt: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof zlangt.ndarray, 'function' );
});

test( 'zlangt: max_norm_4x4', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'max_norm_4x4' );
	dl = new Complex128Array( [ 3, 2, 1, 4, 2, 1 ] );
	d = new Complex128Array( [ 2, 1, 4, 2, 5, 3, 6, 1 ] );
	du = new Complex128Array( [ -1, 3, -2, 1, -3, 2 ] );
	result = base( 'max', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: one_norm_4x4', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_4x4' );
	dl = new Complex128Array( [ 3, 2, 1, 4, 2, 1 ] );
	d = new Complex128Array( [ 2, 1, 4, 2, 5, 3, 6, 1 ] );
	du = new Complex128Array( [ -1, 3, -2, 1, -3, 2 ] );
	result = base( 'one-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: one_norm_O alias same as one-norm', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_O_4x4' );
	dl = new Complex128Array( [ 3, 2, 1, 4, 2, 1 ] );
	d = new Complex128Array( [ 2, 1, 4, 2, 5, 3, 6, 1 ] );
	du = new Complex128Array( [ -1, 3, -2, 1, -3, 2 ] );
	result = base( 'one-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: inf_norm_4x4', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'inf_norm_4x4' );
	dl = new Complex128Array( [ 3, 2, 1, 4, 2, 1 ] );
	d = new Complex128Array( [ 2, 1, 4, 2, 5, 3, 6, 1 ] );
	du = new Complex128Array( [ -1, 3, -2, 1, -3, 2 ] );
	result = base( 'inf-norm', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: frob_norm_4x4', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_4x4' );
	dl = new Complex128Array( [ 3, 2, 1, 4, 2, 1 ] );
	d = new Complex128Array( [ 2, 1, 4, 2, 5, 3, 6, 1 ] );
	du = new Complex128Array( [ -1, 3, -2, 1, -3, 2 ] );
	result = base( 'frobenius', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: frob_norm_E alias same as frobenius', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_E_4x4' );
	dl = new Complex128Array( [ 3, 2, 1, 4, 2, 1 ] );
	d = new Complex128Array( [ 2, 1, 4, 2, 5, 3, 6, 1 ] );
	du = new Complex128Array( [ -1, 3, -2, 1, -3, 2 ] );
	result = base( 'frobenius', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: max_norm_n1', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'max_norm_n1' );
	dl = new Complex128Array( 0 );
	d = new Complex128Array( [ -7, 3 ] );
	du = new Complex128Array( 0 );
	result = base( 'max', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: one_norm_n1', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_n1' );
	dl = new Complex128Array( 0 );
	d = new Complex128Array( [ -7, 3 ] );
	du = new Complex128Array( 0 );
	result = base( 'one-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: inf_norm_n1', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'inf_norm_n1' );
	dl = new Complex128Array( 0 );
	d = new Complex128Array( [ -7, 3 ] );
	du = new Complex128Array( 0 );
	result = base( 'inf-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: frob_norm_n1', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_n1' );
	dl = new Complex128Array( 0 );
	d = new Complex128Array( [ -7, 3 ] );
	du = new Complex128Array( 0 );
	result = base( 'frobenius', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: n_zero', function t() {
	var result;
	var dl;
	var du;
	var d;

	dl = new Complex128Array( 0 );
	d = new Complex128Array( 0 );
	du = new Complex128Array( 0 );
	result = base( 'max', 0, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'zlangt: max_norm_5x5', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'max_norm_5x5' );
	dl = new Complex128Array( [ 1, 2, 2, 3, 3, 1, 4, 5 ] );
	d = new Complex128Array( [ 10, 1, 20, 2, 30, 3, 40, 4, 50, 5 ] );
	du = new Complex128Array( [ 5, 6, 6, 7, 7, 8, 8, 9 ] );
	result = base( 'max', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: one_norm_5x5', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_5x5' );
	dl = new Complex128Array( [ 1, 2, 2, 3, 3, 1, 4, 5 ] );
	d = new Complex128Array( [ 10, 1, 20, 2, 30, 3, 40, 4, 50, 5 ] );
	du = new Complex128Array( [ 5, 6, 6, 7, 7, 8, 8, 9 ] );
	result = base( 'one-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: inf_norm_5x5', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'inf_norm_5x5' );
	dl = new Complex128Array( [ 1, 2, 2, 3, 3, 1, 4, 5 ] );
	d = new Complex128Array( [ 10, 1, 20, 2, 30, 3, 40, 4, 50, 5 ] );
	du = new Complex128Array( [ 5, 6, 6, 7, 7, 8, 8, 9 ] );
	result = base( 'inf-norm', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: frob_norm_5x5', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_5x5' );
	dl = new Complex128Array( [ 1, 2, 2, 3, 3, 1, 4, 5 ] );
	d = new Complex128Array( [ 10, 1, 20, 2, 30, 3, 40, 4, 50, 5 ] );
	du = new Complex128Array( [ 5, 6, 6, 7, 7, 8, 8, 9 ] );
	result = base( 'frobenius', 5, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: max_norm_n2', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'max_norm_n2' );
	dl = new Complex128Array( [ 0.5, 1.0 ] );
	d = new Complex128Array( [ 3, 2, 4, 1 ] );
	du = new Complex128Array( [ 1.5, 0.5 ] );
	result = base( 'max', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: one_norm_n2', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'one_norm_n2' );
	dl = new Complex128Array( [ 0.5, 1.0 ] );
	d = new Complex128Array( [ 3, 2, 4, 1 ] );
	du = new Complex128Array( [ 1.5, 0.5 ] );
	result = base( 'one-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: inf_norm_n2', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'inf_norm_n2' );
	dl = new Complex128Array( [ 0.5, 1.0 ] );
	d = new Complex128Array( [ 3, 2, 4, 1 ] );
	du = new Complex128Array( [ 1.5, 0.5 ] );
	result = base( 'inf-norm', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt: frob_norm_n2', function t() {
	var result;
	var tc;
	var dl;
	var du;
	var d;

	tc = findCase( 'frob_norm_n2' );
	dl = new Complex128Array( [ 0.5, 1.0 ] );
	d = new Complex128Array( [ 3, 2, 4, 1 ] );
	du = new Complex128Array( [ 1.5, 0.5 ] );
	result = base( 'frobenius', 2, dl, 1, 0, d, 1, 0, du, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlangt.ndarray: throws on invalid norm', function t() {
	var dl;
	var du;
	var d;

	dl = new Complex128Array( 0 );
	d = new Complex128Array( [ 1, 0 ] );
	du = new Complex128Array( 0 );
	assert.throws( function badNorm() {
		zlangt.ndarray( 'bad-norm', 1, dl, 1, 0, d, 1, 0, du, 1, 0 );
	}, /invalid argument/ );
});
