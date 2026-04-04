/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines-per-function */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zpprfs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zpprfs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync, max-len
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
* Converts a Float64Array to an array.
*
* @private
* @param {Float64Array} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out;
	var i;

	out = [];
	for ( i = 0; i < arr.length; i += 1 ) {
		out.push( arr[ i ] );
	}
	return out;
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {Array} actual - actual value
* @param {Array} expected - expected value
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
* Creates a Complex128Array from interleaved real/imaginary doubles.
*
* @private
* @param {Array} arr - interleaved real/imaginary values
* @returns {Complex128Array} complex array
*/
function c128( arr ) {
	return new Complex128Array( new Float64Array( arr ) );
}


// TESTS //

test( 'zpprfs is a function', function t() {
	assert.equal( typeof zpprfs, 'function' );
});

test( 'zpprfs: basic_upper_3x3', function t() {
	var RWORK;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var afp;
	var tc;
	var ap;
	var xv;
	var b;
	var x;

	tc = findCase( 'basic_upper_3x3' );
	ap = c128( tc.AP );
	afp = c128( tc.AFP );
	b = c128( tc.B );
	x = c128( tc.x );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Complex128Array( 6 );
	RWORK = new Float64Array( 3 );
	info = zpprfs( 'upper', 3, 1, ap, 1, 0, afp, 1, 0, b, 1, 3, 0, x, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
	xv = toArray( new Float64Array( x.buffer, x.byteOffset, 6 ) );
	assertArrayClose( xv, tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( FERR ), tc.ferr, 1e-6, 'ferr' );
	assertArrayClose( toArray( BERR ), tc.berr, 1e-6, 'berr' );
});

test( 'zpprfs: basic_lower_3x3', function t() {
	var RWORK;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var afp;
	var tc;
	var ap;
	var xv;
	var b;
	var x;

	tc = findCase( 'basic_lower_3x3' );
	ap = c128( tc.AP );
	afp = c128( tc.AFP );
	b = c128( tc.B );
	x = c128( tc.x );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Complex128Array( 6 );
	RWORK = new Float64Array( 3 );
	info = zpprfs( 'lower', 3, 1, ap, 1, 0, afp, 1, 0, b, 1, 3, 0, x, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
	xv = toArray( new Float64Array( x.buffer, x.byteOffset, 6 ) );
	assertArrayClose( xv, tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( FERR ), tc.ferr, 1e-6, 'ferr' );
	assertArrayClose( toArray( BERR ), tc.berr, 1e-6, 'berr' );
});

test( 'zpprfs: multi_rhs_3x3', function t() {
	var RWORK;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var afp;
	var tc;
	var ap;
	var xv;
	var b;
	var x;

	tc = findCase( 'multi_rhs_3x3' );
	ap = c128( tc.AP );
	afp = c128( tc.AFP );
	b = c128( tc.B );
	x = c128( tc.x );
	FERR = new Float64Array( 2 );
	BERR = new Float64Array( 2 );
	WORK = new Complex128Array( 6 );
	RWORK = new Float64Array( 3 );
	info = zpprfs( 'upper', 3, 2, ap, 1, 0, afp, 1, 0, b, 1, 3, 0, x, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
	xv = toArray( new Float64Array( x.buffer, x.byteOffset, 12 ) );
	assertArrayClose( xv, tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( FERR ), tc.ferr, 1e-6, 'ferr' );
	assertArrayClose( toArray( BERR ), tc.berr, 1e-6, 'berr' );
});

test( 'zpprfs: n_zero', function t() {
	var RWORK;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var afp;
	var ap;
	var b;
	var x;

	ap = c128( [ 1.0, 0.0 ] );
	afp = c128( [ 1.0, 0.0 ] );
	b = c128( [ 1.0, 0.0 ] );
	x = c128( [ 1.0, 0.0 ] );
	FERR = new Float64Array( [ -1.0 ] );
	BERR = new Float64Array( [ -1.0 ] );
	WORK = new Complex128Array( 2 );
	RWORK = new Float64Array( 1 );
	info = zpprfs( 'upper', 0, 1, ap, 1, 0, afp, 1, 0, b, 1, 1, 0, x, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
	assert.equal( FERR[ 0 ], 0.0 );
	assert.equal( BERR[ 0 ], 0.0 );
});

test( 'zpprfs: nrhs_zero', function t() {
	var RWORK;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var afp;
	var ap;
	var b;
	var x;

	ap = c128( [ 1.0, 0.0 ] );
	afp = c128( [ 1.0, 0.0 ] );
	b = c128( [ 1.0, 0.0 ] );
	x = c128( [ 1.0, 0.0 ] );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Complex128Array( 2 );
	RWORK = new Float64Array( 1 );
	info = zpprfs( 'upper', 3, 0, ap, 1, 0, afp, 1, 0, b, 1, 3, 0, x, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
});

test( 'zpprfs: n_one', function t() {
	var RWORK;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var afp;
	var tc;
	var ap;
	var xv;
	var b;
	var x;

	tc = findCase( 'n_one' );
	ap = c128( tc.AP );
	afp = c128( tc.AFP );
	b = c128( tc.B );
	x = c128( tc.x );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Complex128Array( 2 );
	RWORK = new Float64Array( 1 );
	info = zpprfs( 'upper', 1, 1, ap, 1, 0, afp, 1, 0, b, 1, 1, 0, x, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
	xv = toArray( new Float64Array( x.buffer, x.byteOffset, 2 ) );
	assertArrayClose( xv, tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( FERR ), tc.ferr, 1e-6, 'ferr' );
	assertArrayClose( toArray( BERR ), tc.berr, 1e-6, 'berr' );
});
