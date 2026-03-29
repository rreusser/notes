/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var ilazlr = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'ilazlr.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );

// Fortran returns 1-based; JS returns 0-based. Convert:
/**
* Expected.
*
* @private
* @param {*} tc - tc
* @returns {*} result
*/
function expected( tc ) {
	return tc.result - 1;
}

// Helper: build Complex128Array from flat interleaved doubles
/**
* C128.
*
* @private
* @param {TypedArray} arr - input array
* @returns {*} result
*/
function c128( arr ) {
	return new Complex128Array( arr );
}


// TESTS //

test( 'ilazlr: main export is a function', function t() {
	assert.strictEqual( typeof ilazlr, 'function' );
});

test( 'ilazlr: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof ilazlr.ndarray, 'function' );
});

test( 'ilazlr: diagonal 3x3 matrix -> last non-zero row = 2 (0-based)', function t() { // eslint-disable-line max-len
	var buf;
	var tc;
	var A;

	tc = fixture.find( function find( t ) {
		return t.name === 'ilazlr_diag';
	} );
	buf = new Float64Array( 2 * 4 * 3 );
	buf[ 0 * 2 + 0 * 8 ] = 1.0;
	buf[ 1 * 2 + 1 * 8 ] = 2.0;
	buf[ 2 * 2 + 2 * 8 ] = 3.0;
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: last row all zeros -> returns 1 (0-based row 2)', function t() {
	var buf;
	var tc;
	var A;

	tc = fixture.find( function find( t ) {
		return t.name === 'ilazlr_row2';
	} );
	buf = new Float64Array( 2 * 4 * 3 );
	buf[ 0 * 2 + 0 * 8 ] = 1.0;
	buf[ 1 * 2 + 1 * 8 ] = 2.0;
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: all zeros -> returns -1', function t() {
	var buf;
	var tc;
	var A;

	tc = fixture.find( function find( t ) {
		return t.name === 'ilazlr_zeros';
	} );
	buf = new Float64Array( 2 * 4 * 3 );
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: M=0 -> returns -1', function t() {
	var buf;
	var tc;
	var A;

	tc = fixture.find( function find( t ) {
		return t.name === 'ilazlr_m_zero';
	} );
	buf = new Float64Array( 2 * 4 * 3 );
	A = c128( buf );
	assert.strictEqual( base( 0, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: only imaginary part non-zero in last row', function t() {
	var buf;
	var tc;
	var A;

	tc = fixture.find( function find( t ) {
		return t.name === 'ilazlr_imag';
	} );
	buf = new Float64Array( 2 * 4 * 3 );
	buf[ 2 * 2 + 1 * 8 + 1 ] = 5.0;
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: full matrix -> returns M-1', function t() {
	var vals;
	var buf;
	var tc;
	var i;
	var j;
	var k;
	var A;

	tc = fixture.find( function find( t ) {
		return t.name === 'ilazlr_full';
	} );
	buf = new Float64Array( 2 * 4 * 3 );
	vals = [
		[1, 1],
		[2, 2],
		[3, 3],
		[4, 4],
		[5, 5],
		[6, 6],
		[7, 7],
		[8, 8],
		[9, 9]
	];
	k = 0;
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			buf[ i * 2 + j * 8 ] = vals[ k ][ 0 ];
			buf[ i * 2 + j * 8 + 1 ] = vals[ k ][ 1 ];
			k++;
		}
	}
	A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: 1x1 non-zero -> returns 0', function t() {
	var buf;
	var tc;
	var A;

	tc = fixture.find( function find( t ) {
		return t.name === 'ilazlr_1x1';
	} );
	buf = new Float64Array( 2 * 4 );
	buf[ 0 ] = 1.0;
	A = c128( buf );
	assert.strictEqual( base( 1, 1, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: 1x1 zero -> returns -1', function t() {
	var buf;
	var tc;
	var A;

	tc = fixture.find( function find( t ) {
		return t.name === 'ilazlr_1x1_zero';
	} );
	buf = new Float64Array( 2 * 4 );
	A = c128( buf );
	assert.strictEqual( base( 1, 1, A, 1, 4, 0 ), expected( tc ) );
});
