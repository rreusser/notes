/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasq1 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlasq1.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	if ( expected === 0.0 ) {
		assert.ok( Math.abs( actual ) <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
		return;
	}
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
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dlasq1: n0 - quick return', function t() {
	var work;
	var info;
	var d;
	var e;

	work = new Float64Array( 4 );
	d = new Float64Array( 1 );
	e = new Float64Array( 1 );
	info = dlasq1( 0, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, 0 );
});

test( 'dlasq1: n1 - single positive diagonal', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n1' );
	work = new Float64Array( 4 );
	d = new Float64Array( [ 5.0 ] );
	e = new Float64Array( 1 );
	info = dlasq1( 1, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n1_neg - single negative diagonal (abs)', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n1_neg' );
	work = new Float64Array( 4 );
	d = new Float64Array( [ -3.0 ] );
	e = new Float64Array( 1 );
	info = dlasq1( 1, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n2 - two elements', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n2' );
	work = new Float64Array( 8 );
	d = new Float64Array( [ 4.0, 3.0 ] );
	e = new Float64Array( [ 1.0 ] );
	info = dlasq1( 2, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n2_neg - two negative diagonals', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n2_neg' );
	work = new Float64Array( 8 );
	d = new Float64Array( [ -4.0, -3.0 ] );
	e = new Float64Array( [ 2.0 ] );
	info = dlasq1( 2, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n3_basic', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n3_basic' );
	work = new Float64Array( 100 );
	d = new Float64Array( [ 4.0, 3.0, 2.0 ] );
	e = new Float64Array( [ 1.0, 0.5 ] );
	info = dlasq1( 3, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n4_basic', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n4_basic' );
	work = new Float64Array( 100 );
	d = new Float64Array( [ 5.0, 4.0, 3.0, 2.0 ] );
	e = new Float64Array( [ 1.0, 0.5, 0.3 ] );
	info = dlasq1( 4, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n3_diag - diagonal matrix (all e=0)', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n3_diag' );
	work = new Float64Array( 100 );
	d = new Float64Array( [ 1.0, 5.0, 3.0 ] );
	e = new Float64Array( [ 0.0, 0.0 ] );
	info = dlasq1( 3, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n5_basic', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n5_basic' );
	work = new Float64Array( 100 );
	d = new Float64Array( [ 6.0, 5.0, 4.0, 3.0, 2.0 ] );
	e = new Float64Array( [ 1.0, 0.8, 0.5, 0.3 ] );
	info = dlasq1( 5, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n2_diag - diagonal N=2 (e=0)', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n2_diag' );
	work = new Float64Array( 100 );
	d = new Float64Array( [ 3.0, 7.0 ] );
	e = new Float64Array( [ 0.0 ] );
	info = dlasq1( 2, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n3_neg_large_e', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n3_neg_large_e' );
	work = new Float64Array( 100 );
	d = new Float64Array( [ -3.0, -5.0, -2.0 ] );
	e = new Float64Array( [ 4.0, 3.0 ] );
	info = dlasq1( 3, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dlasq1: n4_identity_like', function t() {
	var work;
	var info;
	var tc;
	var d;
	var e;

	tc = findCase( 'n4_identity_like' );
	work = new Float64Array( 100 );
	d = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	e = new Float64Array( [ 0.01, 0.01, 0.01 ] );
	info = dlasq1( 4, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

// Error case: negative N
test( 'dlasq1: negative N returns -1', function t() {
	var work;
	var info;
	var d;
	var e;

	work = new Float64Array( 4 );
	d = new Float64Array( 1 );
	e = new Float64Array( 1 );
	info = dlasq1( -1, d, 1, 0, e, 1, 0, work, 1, 0 );
	assert.strictEqual( info, -1 );
});
