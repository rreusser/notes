/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlanst = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlanst.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'dlanst: N=0 returns 0', function t() {
	var result;
	var d;
	var e;

	d = new Float64Array( 0 );
	e = new Float64Array( 0 );
	result = dlanst( 'max', 0, d, 1, 0, e, 1, 0 );
	assert.strictEqual( result, 0.0 );
});

test( 'dlanst: N=1, max norm', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n1_max' );
	d = new Float64Array( [ tc.d1 ] );
	e = new Float64Array( 0 );
	result = dlanst( 'max', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=1, one-norm', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n1_one' );
	d = new Float64Array( [ tc.d1 ] );
	e = new Float64Array( 0 );
	result = dlanst( 'one-norm', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=1, infinity-norm', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n1_inf' );
	d = new Float64Array( [ tc.d1 ] );
	e = new Float64Array( 0 );
	result = dlanst( 'inf-norm', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=1, Frobenius norm', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n1_frob' );
	d = new Float64Array( [ tc.d1 ] );
	e = new Float64Array( 0 );
	result = dlanst( 'frobenius', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, max norm', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n5_max' );
	d = new Float64Array( tc.d );
	e = new Float64Array( tc.e );
	result = dlanst( 'max', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, one-norm (O)', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n5_one' );
	d = new Float64Array( tc.d );
	e = new Float64Array( tc.e );
	result = dlanst( 'one-norm', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, infinity-norm', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n5_inf' );
	d = new Float64Array( tc.d );
	e = new Float64Array( tc.e );
	result = dlanst( 'inf-norm', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, Frobenius norm', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n5_frob' );
	d = new Float64Array( tc.d );
	e = new Float64Array( tc.e );
	result = dlanst( 'frobenius', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=2, one-norm (1)', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n2_one' );
	d = new Float64Array( [ tc.d1, tc.d2 ] );
	e = new Float64Array( [ tc.e1 ] );
	result = dlanst( 'one-norm', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=2, Frobenius norm', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n2_frob' );
	d = new Float64Array( [ tc.d1, tc.d2 ] );
	e = new Float64Array( [ tc.e1 ] );
	result = dlanst( 'frobenius', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: E norm type (same as F)', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n2_e_norm' );
	d = new Float64Array( [ tc.d1, tc.d2 ] );
	e = new Float64Array( [ tc.e1 ] );
	result = dlanst( 'frobenius', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: N=5, max norm, all positive', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = findCase( 'n5_max_positive' );
	d = new Float64Array( tc.d );
	e = new Float64Array( tc.e );
	result = dlanst( 'max', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.anorm, 1e-14, 'anorm' );
});

test( 'dlanst: max norm where off-diagonal has largest element', function t() {
	var result;
	var d;
	var e;

	d = new Float64Array( [ 1.0, 2.0 ] );
	e = new Float64Array( [ 10.0 ] );
	result = dlanst( 'max', 2, d, 1, 0, e, 1, 0 );
	assert.strictEqual( result, 10.0 );
});

test( 'dlanst: supports stride and offset for d and e', function t() {
	var result;
	var d;
	var e;

	d = new Float64Array( [ 99.0, 2.0, 99.0, -4.0, 99.0, 6.0 ] );
	e = new Float64Array( [ 1.0, 99.0, -2.0 ] );
	result = dlanst( 'max', 3, d, 2, 1, e, 2, 0 );
	assert.strictEqual( result, 6.0 );
});
