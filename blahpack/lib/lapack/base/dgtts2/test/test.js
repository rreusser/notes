/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgttrf = require( './../../dgttrf/lib/base.js' );
var dgtts2 = require( './../lib/base.js' );


// FIXTURES //

// We reuse the dgttrs fixture since dgttrs just calls dgtts2
var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dgttrs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Factorize.
*
* @private
* @param {*} dlArr - dlArr
* @param {*} dArr - dArr
* @param {*} duArr - duArr
* @param {*} n - n
* @returns {*} result
*/
function factorize( dlArr, dArr, duArr, n ) {
	var ipiv = new Int32Array( n );
	var info;
	var du2 = new Float64Array( Math.max( n - 2, 0 ) );
	var dl = new Float64Array( dlArr );
	var du = new Float64Array( duArr );
	var d = new Float64Array( dArr );

	info = dgttrf( n, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );
	return {
		'dl': dl,
		'd': d,
		'du': du,
		'du2': du2,
		'ipiv': ipiv,
		'info': info
	};
}


// TESTS //

test( 'dgtts2: itrans=0 (no transpose), single RHS', function t() {
	var tc;
	var n;
	var f;
	var B;

	tc = findCase( 'notrans_single_rhs' );
	n = 5;
	f = factorize( [ -1, -1, -1, -1 ], [ 2, 2, 2, 2, 2 ], [ -1, -1, -1, -1 ], n );
	B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	dgtts2( 0, n, 1, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 ); // eslint-disable-line max-len
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgtts2: itrans=1 (transpose), single RHS', function t() {
	var tc;
	var n;
	var f;
	var B;

	tc = findCase( 'trans_single_rhs' );
	n = 5;
	f = factorize( [ -1, -1, -1, -1 ], [ 2, 2, 2, 2, 2 ], [ -1, -1, -1, -1 ], n );
	B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	dgtts2( 1, n, 1, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 ); // eslint-disable-line max-len
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgtts2: itrans=2 (conjugate transpose = transpose for real)', function t() { // eslint-disable-line max-len
	var tc;
	var n;
	var f;
	var B;

	tc = findCase( 'trans_single_rhs' );
	n = 5;
	f = factorize( [ -1, -1, -1, -1 ], [ 2, 2, 2, 2, 2 ], [ -1, -1, -1, -1 ], n );
	B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	dgtts2( 2, n, 1, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 ); // eslint-disable-line max-len
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgtts2: multiple RHS, no transpose', function t() {
	var tc;
	var n;
	var f;
	var B;

	tc = findCase( 'notrans_multi_rhs' );
	n = 5;
	f = factorize( [ -1, -1, -1, -1 ], [ 2, 2, 2, 2, 2 ], [ -1, -1, -1, -1 ], n );
	B = new Float64Array([
		1,
		0,
		0,
		0,
		1,
		1,
		1,
		1,
		1,
		1,
		0,
		0,
		1,
		0,
		0
	]);
	dgtts2( 0, n, 3, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 ); // eslint-disable-line max-len
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgtts2: pivoting, no-transpose', function t() {
	var tc;
	var n;
	var f;
	var B;

	tc = findCase( 'pivot_notrans' );
	n = 5;
	f = factorize( [ 10, 10, 10, 10 ], [ 1, 1, 1, 1, 1 ], [ 2, 2, 2, 2 ], n );
	B = new Float64Array( [ 3, 13, 13, 13, 11 ] );
	dgtts2( 0, n, 1, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 ); // eslint-disable-line max-len
	assertArrayClose( B, new Float64Array( tc.B ), 1e-13, 'B' );
});

test( 'dgtts2: pivoting, transpose', function t() {
	var tc;
	var n;
	var f;
	var B;

	tc = findCase( 'pivot_trans' );
	n = 5;
	f = factorize( [ 10, 10, 10, 10 ], [ 1, 1, 1, 1, 1 ], [ 2, 2, 2, 2 ], n );
	B = new Float64Array( [ 3, 13, 13, 13, 11 ] );
	dgtts2( 1, n, 1, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 ); // eslint-disable-line max-len
	assertArrayClose( B, new Float64Array( tc.B ), 1e-13, 'B' );
});

test( 'dgtts2: N=0 quick return', function t() {
	dgtts2( 0, 0, 1, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Int32Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 1, 0 ); // eslint-disable-line max-len

	// No crash = pass
	assert.ok( true );
});

test( 'dgtts2: NRHS=0 quick return', function t() {
	dgtts2( 0, 5, 0, new Float64Array( 4 ), 1, 0, new Float64Array( 5 ), 1, 0, new Float64Array( 4 ), 1, 0, new Float64Array( 3 ), 1, 0, new Int32Array( 5 ), 1, 0, new Float64Array( 0 ), 1, 5, 0 ); // eslint-disable-line max-len
	assert.ok( true );
});

test( 'dgtts2: N=1', function t() {
	var tc;
	var n;
	var f;
	var B;

	tc = findCase( 'n_one' );
	n = 1;
	f = factorize( [], [ 5 ], [], n );
	B = new Float64Array( [ 10 ] );
	dgtts2( 0, n, 1, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 ); // eslint-disable-line max-len
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgtts2: N=2, no-transpose', function t() {
	var tc;
	var n;
	var f;
	var B;

	tc = findCase( 'n_two_notrans' );
	n = 2;
	f = factorize( [ 3 ], [ 4, 7 ], [ 1 ], n );
	B = new Float64Array( [ 5, 10 ] );
	dgtts2( 0, n, 1, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 ); // eslint-disable-line max-len
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgtts2: N=2, transpose', function t() {
	var tc;
	var n;
	var f;
	var B;

	tc = findCase( 'n_two_trans' );
	n = 2;
	f = factorize( [ 3 ], [ 4, 7 ], [ 1 ], n );
	B = new Float64Array( [ 5, 10 ] );
	dgtts2( 1, n, 1, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 ); // eslint-disable-line max-len
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});
