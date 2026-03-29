/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlarra = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlarra.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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


// TESTS //

test( 'dlarra is a function', function t() {
	assert.equal( typeof dlarra, 'function' );
});

test( 'dlarra: n_zero (quick return)', function t() {
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var tc;
	var d;
	var e;

	ISPLIT = new Int32Array( 1 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( 1 );
	e = new Float64Array( 1 );
	E2 = new Float64Array( 1 );
	tc = findCase( 'n_zero' );
	info = dlarra( 0, d, 1, 0, e, 1, 0, E2, 1, 0, 1.0, 1.0, nsplit, ISPLIT, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assert.equal( nsplit[ 0 ], tc.nsplit, 'nsplit' );
});

test( 'dlarra: n_one (single element)', function t() {
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var tc;
	var d;
	var e;

	ISPLIT = new Int32Array( 1 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ 5.0 ] );
	e = new Float64Array( [ 0.0 ] );
	E2 = new Float64Array( [ 0.0 ] );
	tc = findCase( 'n_one' );
	info = dlarra( 1, d, 1, 0, e, 1, 0, E2, 1, 0, 1.0, 5.0, nsplit, ISPLIT, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assert.equal( nsplit[ 0 ], tc.nsplit, 'nsplit' );
	assert.equal( ISPLIT[ 0 ], tc.isplit[ 0 ], 'isplit[0]' );
});

test( 'dlarra: spltol_neg_no_split (absolute threshold, no splits)', function t() { // eslint-disable-line max-len
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var tc;
	var d;
	var e;
	var i;

	ISPLIT = new Int32Array( 4 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	e = new Float64Array( [ 1.0, 1.0, 1.0, 0.0 ] );
	E2 = new Float64Array( [ 1.0, 1.0, 1.0, 0.0 ] );
	tc = findCase( 'spltol_neg_no_split' );
	info = dlarra( 4, d, 1, 0, e, 1, 0, E2, 1, 0, -0.1, 5.0, nsplit, ISPLIT, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assert.equal( nsplit[ 0 ], tc.nsplit, 'nsplit' );
	for ( i = 0; i < tc.isplit.length; i += 1 ) {
		assert.equal( ISPLIT[ i ], tc.isplit[ i ], 'isplit[' + i + ']' );
	}
	for ( i = 0; i < tc.e.length; i += 1 ) {
		assert.equal( e[ i ], tc.e[ i ], 'e[' + i + ']' );
	}
	for ( i = 0; i < tc.e2.length; i += 1 ) {
		assert.equal( E2[ i ], tc.e2[ i ], 'e2[' + i + ']' );
	}
});

test( 'dlarra: spltol_neg_with_splits (absolute threshold, splits at 1 and 3)', function t() { // eslint-disable-line max-len
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var tc;
	var d;
	var e;
	var i;

	ISPLIT = new Int32Array( 4 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	e = new Float64Array( [ 0.01, 1.0, 0.02, 0.0 ] );
	E2 = new Float64Array( [ 0.0001, 1.0, 0.0004, 0.0 ] );
	tc = findCase( 'spltol_neg_with_splits' );
	info = dlarra( 4, d, 1, 0, e, 1, 0, E2, 1, 0, -0.1, 5.0, nsplit, ISPLIT, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assert.equal( nsplit[ 0 ], tc.nsplit, 'nsplit' );
	for ( i = 0; i < tc.isplit.length; i += 1 ) {
		assert.equal( ISPLIT[ i ], tc.isplit[ i ], 'isplit[' + i + ']' );
	}
	for ( i = 0; i < tc.e.length; i += 1 ) {
		assert.equal( e[ i ], tc.e[ i ], 'e[' + i + ']' );
	}
	for ( i = 0; i < tc.e2.length; i += 1 ) {
		assert.equal( E2[ i ], tc.e2[ i ], 'e2[' + i + ']' );
	}
});

test( 'dlarra: spltol_pos_no_split (relative accuracy, no splits)', function t() { // eslint-disable-line max-len
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var tc;
	var d;
	var e;
	var i;

	ISPLIT = new Int32Array( 4 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	e = new Float64Array( [ 2.0, 1.5, 2.5, 0.0 ] );
	E2 = new Float64Array( [ 4.0, 2.25, 6.25, 0.0 ] );
	tc = findCase( 'spltol_pos_no_split' );
	info = dlarra( 4, d, 1, 0, e, 1, 0, E2, 1, 0, 0.01, 5.0, nsplit, ISPLIT, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assert.equal( nsplit[ 0 ], tc.nsplit, 'nsplit' );
	for ( i = 0; i < tc.isplit.length; i += 1 ) {
		assert.equal( ISPLIT[ i ], tc.isplit[ i ], 'isplit[' + i + ']' );
	}
	for ( i = 0; i < tc.e.length; i += 1 ) {
		assert.equal( e[ i ], tc.e[ i ], 'e[' + i + ']' );
	}
	for ( i = 0; i < tc.e2.length; i += 1 ) {
		assert.equal( E2[ i ], tc.e2[ i ], 'e2[' + i + ']' );
	}
});

test( 'dlarra: spltol_pos_with_splits (relative accuracy, splits at 1 and 3)', function t() { // eslint-disable-line max-len
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var tc;
	var d;
	var e;
	var i;

	ISPLIT = new Int32Array( 4 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ 4.0, 3.0, 2.0, 5.0 ] );
	e = new Float64Array( [ 0.001, 1.0, 0.001, 0.0 ] );
	E2 = new Float64Array( [ 0.000001, 1.0, 0.000001, 0.0 ] );
	tc = findCase( 'spltol_pos_with_splits' );
	info = dlarra( 4, d, 1, 0, e, 1, 0, E2, 1, 0, 0.01, 5.0, nsplit, ISPLIT, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assert.equal( nsplit[ 0 ], tc.nsplit, 'nsplit' );
	for ( i = 0; i < tc.isplit.length; i += 1 ) {
		assert.equal( ISPLIT[ i ], tc.isplit[ i ], 'isplit[' + i + ']' );
	}
	for ( i = 0; i < tc.e.length; i += 1 ) {
		assert.equal( e[ i ], tc.e[ i ], 'e[' + i + ']' );
	}
	for ( i = 0; i < tc.e2.length; i += 1 ) {
		assert.equal( E2[ i ], tc.e2[ i ], 'e2[' + i + ']' );
	}
});

test( 'dlarra: all_zero_offdiag (all off-diagonals zero, N splits)', function t() { // eslint-disable-line max-len
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var tc;
	var d;
	var e;
	var i;

	ISPLIT = new Int32Array( 5 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	e = new Float64Array( [ 0.0, 0.0, 0.0, 0.0, 0.0 ] );
	E2 = new Float64Array( [ 0.0, 0.0, 0.0, 0.0, 0.0 ] );
	tc = findCase( 'all_zero_offdiag' );
	info = dlarra( 5, d, 1, 0, e, 1, 0, E2, 1, 0, -0.1, 5.0, nsplit, ISPLIT, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assert.equal( nsplit[ 0 ], tc.nsplit, 'nsplit' );
	for ( i = 0; i < tc.isplit.length; i += 1 ) {
		assert.equal( ISPLIT[ i ], tc.isplit[ i ], 'isplit[' + i + ']' );
	}
});

test( 'dlarra: larger_6x6_neg_spltol (6x6 with mixed splits)', function t() {
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var tc;
	var d;
	var e;
	var i;

	ISPLIT = new Int32Array( 6 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ 10.0, 8.0, 6.0, 4.0, 2.0, 1.0 ] );
	e = new Float64Array( [ 0.001, 5.0, 0.002, 3.0, 0.003, 0.0 ] );
	E2 = new Float64Array( [ 0.000001, 25.0, 0.000004, 9.0, 0.000009, 0.0 ] );
	tc = findCase( 'larger_6x6_neg_spltol' );
	info = dlarra( 6, d, 1, 0, e, 1, 0, E2, 1, 0, -0.01, 10.0, nsplit, ISPLIT, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assert.equal( nsplit[ 0 ], tc.nsplit, 'nsplit' );
	for ( i = 0; i < tc.isplit.length; i += 1 ) {
		assert.equal( ISPLIT[ i ], tc.isplit[ i ], 'isplit[' + i + ']' );
	}
	for ( i = 0; i < tc.e.length; i += 1 ) {
		assert.equal( e[ i ], tc.e[ i ], 'e[' + i + ']' );
	}
	for ( i = 0; i < tc.e2.length; i += 1 ) {
		assert.equal( E2[ i ], tc.e2[ i ], 'e2[' + i + ']' );
	}
});

test( 'dlarra: neg_diag_pos_spltol (negative diagonal, positive spltol)', function t() { // eslint-disable-line max-len
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var tc;
	var d;
	var e;
	var i;

	ISPLIT = new Int32Array( 3 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ -4.0, -9.0, -1.0 ] );
	e = new Float64Array( [ 0.01, 0.01, 0.0 ] );
	E2 = new Float64Array( [ 0.0001, 0.0001, 0.0 ] );
	tc = findCase( 'neg_diag_pos_spltol' );
	info = dlarra( 3, d, 1, 0, e, 1, 0, E2, 1, 0, 0.01, 9.0, nsplit, ISPLIT, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assert.equal( nsplit[ 0 ], tc.nsplit, 'nsplit' );
	for ( i = 0; i < tc.isplit.length; i += 1 ) {
		assert.equal( ISPLIT[ i ], tc.isplit[ i ], 'isplit[' + i + ']' );
	}
	for ( i = 0; i < tc.e.length; i += 1 ) {
		assert.equal( e[ i ], tc.e[ i ], 'e[' + i + ']' );
	}
	for ( i = 0; i < tc.e2.length; i += 1 ) {
		assert.equal( E2[ i ], tc.e2[ i ], 'e2[' + i + ']' );
	}
});

test( 'dlarra: non-unit strides', function t() {
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var d;
	var e;

	ISPLIT = new Int32Array( 6 );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ 4.0, 99.0, 3.0, 99.0, 2.0, 99.0, 5.0 ] );
	e = new Float64Array( [ 0.01, 99.0, 1.0, 99.0, 0.02, 99.0, 0.0 ] );
	E2 = new Float64Array( [ 0.0001, 99.0, 1.0, 99.0, 0.0004, 99.0, 0.0 ] );
	info = dlarra( 4, d, 2, 0, e, 2, 0, E2, 2, 0, -0.1, 5.0, nsplit, ISPLIT, 2, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( nsplit[ 0 ], 3, 'nsplit' );
	assert.equal( ISPLIT[ 0 ], 1, 'isplit[0]' );
	assert.equal( ISPLIT[ 2 ], 3, 'isplit[1] at stride 2' );
	assert.equal( ISPLIT[ 4 ], 4, 'isplit[2] at stride 2 (last = N)' );
	assert.equal( e[ 0 ], 0.0, 'e[0] zeroed' );
	assert.equal( e[ 2 ], 1.0, 'e[2] untouched' );
	assert.equal( e[ 4 ], 0.0, 'e[4] zeroed' );
	assert.equal( E2[ 0 ], 0.0, 'E2[0] zeroed' );
	assert.equal( E2[ 2 ], 1.0, 'E2[2] untouched' );
	assert.equal( E2[ 4 ], 0.0, 'E2[4] zeroed' );
});

test( 'dlarra: offset support', function t() {
	var ISPLIT;
	var nsplit;
	var info;
	var E2;
	var d;
	var e;

	ISPLIT = new Int32Array( [ 99, 0, 0 ] );
	nsplit = new Int32Array( 1 );
	d = new Float64Array( [ 99.0, 5.0 ] );
	e = new Float64Array( [ 99.0, 0.0 ] );
	E2 = new Float64Array( [ 99.0, 0.0 ] );
	info = dlarra( 1, d, 1, 1, e, 1, 1, E2, 1, 1, 1.0, 5.0, nsplit, ISPLIT, 1, 1 );
	assert.equal( info, 0, 'info' );
	assert.equal( nsplit[ 0 ], 1, 'nsplit' );
	assert.equal( ISPLIT[ 1 ], 1, 'isplit[1] = N = 1' );
	assert.equal( ISPLIT[ 0 ], 99, 'isplit[0] untouched' );
});
