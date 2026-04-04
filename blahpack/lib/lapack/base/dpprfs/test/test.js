/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-statements-per-line, require-jsdoc, stdlib/jsdoc-private-annotation, node/no-sync, max-len, max-statements */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dpptrf = require( '../../dpptrf/lib/base.js' );
var dpptrs = require( '../../dpptrs/lib/base.js' );
var dpprfs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dpprfs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Asserts that two values are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
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
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}

/**
* Copies elements from one array to another.
*
* @private
* @param {Float64Array} src - source array
* @param {Float64Array} dst - destination array
*/
function copyArray( src, dst ) {
	var i;
	for ( i = 0; i < src.length; i++ ) {
		dst[ i ] = src[ i ];
	}
}


// TESTS //

test( 'dpprfs is a function', function t() {
	assert.equal( typeof dpprfs, 'function' );
});

test( 'dpprfs: basic_upper_3x3', function t() {
	var IWORK;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var AFP;
	var AP;
	var tc;
	var X;
	var B;

	tc = findCase( 'basic_upper_3x3' );
	AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
	AFP = new Float64Array( 6 );
	copyArray( AP, AFP );
	info = dpptrf( 'upper', 3, AFP, 1, 0 );
	assert.equal( info, 0, 'dpptrf' );
	B = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	X = new Float64Array( 3 );
	copyArray( B, X );
	info = dpptrs( 'upper', 3, 1, AFP, 1, 0, X, 1, 3, 0 );
	assert.equal( info, 0, 'dpptrs' );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 9 );
	IWORK = new Int32Array( 3 );
	info = dpprfs( 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( X, tc.x, 1e-14, 'x' );
	assertArrayClose( FERR, tc.ferr, 0.1, 'ferr' );
	assertArrayClose( BERR, tc.berr, 0.1, 'berr' );
});

test( 'dpprfs: basic_lower_3x3', function t() {
	var IWORK;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var AFP;
	var AP;
	var tc;
	var X;
	var B;

	tc = findCase( 'basic_lower_3x3' );
	AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
	AFP = new Float64Array( 6 );
	copyArray( AP, AFP );
	info = dpptrf( 'lower', 3, AFP, 1, 0 );
	assert.equal( info, 0, 'dpptrf' );
	B = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	X = new Float64Array( 3 );
	copyArray( B, X );
	info = dpptrs( 'lower', 3, 1, AFP, 1, 0, X, 1, 3, 0 );
	assert.equal( info, 0, 'dpptrs' );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 9 );
	IWORK = new Int32Array( 3 );
	info = dpprfs( 'lower', 3, 1, AP, 1, 0, AFP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( X, tc.x, 1e-14, 'x' );
	assertArrayClose( FERR, tc.ferr, 0.1, 'ferr' );
	assertArrayClose( BERR, tc.berr, 0.1, 'berr' );
});

test( 'dpprfs: multi_rhs_3x3', function t() {
	var IWORK;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var xOut;
	var AFP;
	var AP;
	var tc;
	var X;
	var B;

	tc = findCase( 'multi_rhs_3x3' );
	AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
	AFP = new Float64Array( 6 );
	copyArray( AP, AFP );
	info = dpptrf( 'upper', 3, AFP, 1, 0 );
	assert.equal( info, 0, 'dpptrf' );
	B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	X = new Float64Array( 6 );
	copyArray( B, X );
	info = dpptrs( 'upper', 3, 2, AFP, 1, 0, X, 1, 3, 0 );
	assert.equal( info, 0, 'dpptrs' );
	FERR = new Float64Array( 2 );
	BERR = new Float64Array( 2 );
	WORK = new Float64Array( 9 );
	IWORK = new Int32Array( 3 );
	info = dpprfs( 'upper', 3, 2, AP, 1, 0, AFP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	xOut = new Float64Array( X.buffer );
	assertArrayClose( xOut, tc.x, 1e-14, 'x' );
	assertArrayClose( FERR, tc.ferr, 0.1, 'ferr' );
	assertArrayClose( BERR, tc.berr, 0.1, 'berr' );
});

test( 'dpprfs: n_zero', function t() {
	var IWORK;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var AFP;
	var AP;
	var tc;
	var X;
	var B;

	tc = findCase( 'n_zero' );
	AP = new Float64Array( 1 );
	AFP = new Float64Array( 1 );
	B = new Float64Array( 1 );
	X = new Float64Array( 1 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 3 );
	IWORK = new Int32Array( 1 );
	info = dpprfs( 'upper', 0, 1, AP, 1, 0, AFP, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( FERR[ 0 ], tc.ferr[ 0 ], 1e-14, 'ferr' );
	assertClose( BERR[ 0 ], tc.berr[ 0 ], 1e-14, 'berr' );
});

test( 'dpprfs: nrhs_zero', function t() {
	var IWORK;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var AFP;
	var AP;
	var tc;
	var X;
	var B;

	tc = findCase( 'nrhs_zero' );
	AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
	AFP = new Float64Array( 6 );
	copyArray( AP, AFP );
	B = new Float64Array( 3 );
	X = new Float64Array( 3 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 9 );
	IWORK = new Int32Array( 3 );
	info = dpprfs( 'upper', 3, 0, AP, 1, 0, AFP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
});

test( 'dpprfs: n_one', function t() {
	var IWORK;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var AFP;
	var AP;
	var tc;
	var X;
	var B;

	tc = findCase( 'n_one' );
	AP = new Float64Array( [ 4.0 ] );
	AFP = new Float64Array( 1 );
	copyArray( AP, AFP );
	info = dpptrf( 'upper', 1, AFP, 1, 0 );
	assert.equal( info, 0, 'dpptrf' );
	B = new Float64Array( [ 2.0 ] );
	X = new Float64Array( 1 );
	copyArray( B, X );
	info = dpptrs( 'upper', 1, 1, AFP, 1, 0, X, 1, 1, 0 );
	assert.equal( info, 0, 'dpptrs' );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 3 );
	IWORK = new Int32Array( 1 );
	info = dpprfs( 'upper', 1, 1, AP, 1, 0, AFP, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( X, tc.x, 1e-14, 'x' );
	assertArrayClose( FERR, tc.ferr, 0.5, 'ferr' );
	assertArrayClose( BERR, tc.berr, 0.1, 'berr' );
});

test( 'dpprfs: illcond_5x5', function t() {
	var IWORK;
	var WORK;
	var FERR;
	var BERR;
	var info;
	var AFP;
	var AP;
	var tc;
	var X;
	var B;

	tc = findCase( 'illcond_5x5' );
	AP = new Float64Array([
		100.0,
		10.0,
		100.0,
		1.0,
		10.0,
		100.0,
		0.1,
		1.0,
		10.0,
		100.0,
		0.01,
		0.1,
		1.0,
		10.0,
		100.0
	]);
	AFP = new Float64Array( 15 );
	copyArray( AP, AFP );
	info = dpptrf( 'upper', 5, AFP, 1, 0 );
	assert.equal( info, 0, 'dpptrf' );
	B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	X = new Float64Array( 5 );
	copyArray( B, X );
	info = dpptrs( 'upper', 5, 1, AFP, 1, 0, X, 1, 5, 0 );
	assert.equal( info, 0, 'dpptrs' );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 15 );
	IWORK = new Int32Array( 5 );
	info = dpprfs( 'upper', 5, 1, AP, 1, 0, AFP, 1, 0, B, 1, 5, 0, X, 1, 5, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( X, tc.x, 1e-14, 'x' );
	assertArrayClose( FERR, tc.ferr, 0.5, 'ferr' );
	assertArrayClose( BERR, tc.berr, 0.5, 'berr' );
});
