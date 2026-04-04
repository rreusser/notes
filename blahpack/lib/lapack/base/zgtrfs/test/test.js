/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgttrf = require( './../../zgttrf/lib/base.js' );
var zgttrs = require( './../../zgttrs/lib/base.js' );
var zgtrfs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zgtrfs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'zgtrfs: basic_notrans', function t() {
	var RWORK;
	var nrhs;
	var IPIV;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var DLF;
	var DUF;
	var DU2;
	var tc;
	var DF;
	var Xv;
	var DL;
	var DU;
	var n;
	var d;
	var B;
	var X;

	tc = findCase( 'basic_notrans' );
	n = 4;
	nrhs = 1;
	DL = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
	d = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
	DU = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
	B = new Complex128Array( [ 5, 1.5, 6, 4, 6, 1, 9, -0.5 ] );
	DLF = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
	DF = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
	DUF = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
	DU2 = new Complex128Array( n );
	IPIV = new Int32Array( n );
	zgttrf( n, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	X = new Complex128Array( [ 5, 1.5, 6, 4, 6, 1, 9, -0.5 ] );
	zgttrs( 'no-transpose', n, nrhs, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, X, 2, n * 2, 0 ); // eslint-disable-line max-len
	WORK = new Complex128Array( 2 * n );
	RWORK = new Float64Array( n );
	FERR = new Float64Array( nrhs );
	BERR = new Float64Array( nrhs );
	info = zgtrfs( 'no-transpose', n, nrhs, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	Xv = reinterpret( X, 0 );
	assertArrayClose( toArray( Xv ), tc.x, 1e-12, 'x' );
	assertArrayClose( toArray( FERR ), tc.ferr, 1.0, 'ferr' );
	assert.ok( BERR[ 0 ] < 1e-10, 'berr small' );
});

test( 'zgtrfs: basic_conjtrans', function t() {
	var RWORK;
	var nrhs;
	var IPIV;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var DLF;
	var DUF;
	var DU2;
	var tc;
	var DF;
	var Xv;
	var DL;
	var DU;
	var n;
	var d;
	var B;
	var X;

	tc = findCase( 'basic_conjtrans' );
	n = 4;
	nrhs = 1;
	DL = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
	d = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
	DU = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
	B = new Complex128Array( [ 6, -2, 7, -1.5, 5, -2.5, 8, 0 ] );
	DLF = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
	DF = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
	DUF = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
	DU2 = new Complex128Array( n );
	IPIV = new Int32Array( n );
	zgttrf( n, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	X = new Complex128Array( [ 6, -2, 7, -1.5, 5, -2.5, 8, 0 ] );
	zgttrs( 'conjugate-transpose', n, nrhs, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, X, 2, n * 2, 0 ); // eslint-disable-line max-len
	WORK = new Complex128Array( 2 * n );
	RWORK = new Float64Array( n );
	FERR = new Float64Array( nrhs );
	BERR = new Float64Array( nrhs );
	info = zgtrfs( 'conjugate-transpose', n, nrhs, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	Xv = reinterpret( X, 0 );
	assertArrayClose( toArray( Xv ), tc.x, 1e-12, 'x' );
	assertArrayClose( toArray( FERR ), tc.ferr, 1.0, 'ferr' );
});

test( 'zgtrfs: multi_rhs_notrans', function t() {
	var RWORK;
	var Bdata;
	var Xdata;
	var nrhs;
	var IPIV;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var DLF;
	var DUF;
	var DU2;
	var tc;
	var DF;
	var Xv;
	var DL;
	var DU;
	var n;
	var d;
	var B;
	var X;

	tc = findCase( 'multi_rhs_notrans' );
	n = 4;
	nrhs = 2;
	DL = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
	d = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
	DU = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
	Bdata = new Float64Array( 2 * n * nrhs );
	Bdata[ 0 ] = 5;
	Bdata[ 1 ] = 1.5;
	Bdata[ 2 ] = 6;
	Bdata[ 3 ] = 4;
	Bdata[ 4 ] = 6;
	Bdata[ 5 ] = 1;
	Bdata[ 6 ] = 9;
	Bdata[ 7 ] = -0.5;
	Bdata[ 8 ] = 5.5;
	Bdata[ 9 ] = 5;
	Bdata[ 10 ] = 12;
	Bdata[ 11 ] = 2;
	Bdata[ 12 ] = 4;
	Bdata[ 13 ] = 0;
	Bdata[ 14 ] = 7.25;
	Bdata[ 15 ] = 0.75;
	B = new Complex128Array( Bdata );
	DLF = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
	DF = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
	DUF = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
	DU2 = new Complex128Array( n );
	IPIV = new Int32Array( n );
	zgttrf( n, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	Xdata = new Float64Array( Bdata.slice() );
	X = new Complex128Array( Xdata );
	zgttrs( 'no-transpose', n, nrhs, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, X, 2, n * 2, 0 ); // eslint-disable-line max-len
	WORK = new Complex128Array( 2 * n );
	RWORK = new Float64Array( n );
	FERR = new Float64Array( nrhs );
	BERR = new Float64Array( nrhs );
	info = zgtrfs( 'no-transpose', n, nrhs, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	Xv = reinterpret( X, 0 );
	assertArrayClose( toArray( Xv ).slice( 0, 2 * n ), tc.x1, 1e-12, 'x1' );
	assertArrayClose( toArray( Xv ).slice( 2 * n, 4 * n ), tc.x2, 1e-12, 'x2' );
	assertArrayClose( toArray( FERR ), tc.ferr, 1.0, 'ferr' );
});

test( 'zgtrfs: n_one', function t() {
	var RWORK;
	var nrhs;
	var IPIV;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var DLF;
	var DUF;
	var DU2;
	var tc;
	var DF;
	var Xv;
	var DL;
	var DU;
	var d;
	var B;
	var X;

	tc = findCase( 'n_one' );
	nrhs = 1;
	d = new Complex128Array( [ 3, 2 ] );
	DL = new Complex128Array( 1 );
	DU = new Complex128Array( 1 );
	B = new Complex128Array( [ 3, 2 ] );
	DF = new Complex128Array( [ 3, 2 ] );
	DLF = new Complex128Array( 1 );
	DUF = new Complex128Array( 1 );
	DU2 = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	zgttrf( 1, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	X = new Complex128Array( [ 3, 2 ] );
	zgttrs( 'no-transpose', 1, nrhs, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, X, 2, 2, 0 ); // eslint-disable-line max-len
	WORK = new Complex128Array( 2 );
	RWORK = new Float64Array( 1 );
	FERR = new Float64Array( nrhs );
	BERR = new Float64Array( nrhs );
	info = zgtrfs( 'no-transpose', 1, nrhs, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	Xv = reinterpret( X, 0 );
	assertArrayClose( toArray( Xv ), tc.x, 1e-12, 'x' );
});

test( 'zgtrfs: n_zero', function t() {
	var RWORK;
	var IPIV;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var DLF;
	var DUF;
	var DU2;
	var tc;
	var DF;
	var DL;
	var DU;
	var d;
	var B;
	var X;

	tc = findCase( 'n_zero' );
	DL = new Complex128Array( 1 );
	d = new Complex128Array( 1 );
	DU = new Complex128Array( 1 );
	DLF = new Complex128Array( 1 );
	DF = new Complex128Array( 1 );
	DUF = new Complex128Array( 1 );
	DU2 = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( 1 );
	X = new Complex128Array( 1 );
	WORK = new Complex128Array( 2 );
	RWORK = new Float64Array( 1 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	info = zgtrfs( 'no-transpose', 0, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'zgtrfs: pivot_5x5_notrans', function t() {
	var RWORK;
	var nrhs;
	var IPIV;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var DLF;
	var DUF;
	var DU2;
	var tc;
	var DF;
	var Xv;
	var DL;
	var DU;
	var n;
	var d;
	var B;
	var X;

	tc = findCase( 'pivot_5x5_notrans' );
	n = 5;
	nrhs = 1;
	DL = new Complex128Array( [ 5, 1, 7, -2, 1, 3, 2, 0.5 ] );
	d = new Complex128Array( [ 1, 0.5, 3, 1, 2, -1, 1, 2, 8, 0 ] );
	DU = new Complex128Array( [ 2, -1, 4, 0, 6, 1, 3, -0.5 ] );
	B = new Complex128Array( [ 3, -0.5, 12, 2, 15, -2, 5, 4.5, 10, 0.5 ] );
	DLF = new Complex128Array( [ 5, 1, 7, -2, 1, 3, 2, 0.5 ] );
	DF = new Complex128Array( [ 1, 0.5, 3, 1, 2, -1, 1, 2, 8, 0 ] );
	DUF = new Complex128Array( [ 2, -1, 4, 0, 6, 1, 3, -0.5 ] );
	DU2 = new Complex128Array( n );
	IPIV = new Int32Array( n );
	zgttrf( n, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	X = new Complex128Array( [ 3, -0.5, 12, 2, 15, -2, 5, 4.5, 10, 0.5 ] );
	zgttrs( 'no-transpose', n, nrhs, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, X, 2, n * 2, 0 ); // eslint-disable-line max-len
	WORK = new Complex128Array( 2 * n );
	RWORK = new Float64Array( n );
	FERR = new Float64Array( nrhs );
	BERR = new Float64Array( nrhs );
	info = zgtrfs( 'no-transpose', n, nrhs, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	Xv = reinterpret( X, 0 );
	assertArrayClose( toArray( Xv ), tc.x, 1e-12, 'x' );
	assertArrayClose( toArray( FERR ), tc.ferr, 1.0, 'ferr' );
});
