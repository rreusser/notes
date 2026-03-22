'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var ilazlr = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ilazlr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// Fortran returns 1-based; JS returns 0-based. Convert:
function expected( tc ) {
	return tc.result - 1;
}

// Helper: build Complex128Array from flat interleaved doubles
function c128( arr ) {
	return new Complex128Array( arr );
}

// TESTS //

test( 'ilazlr: main export is a function', function t() {
	assert.strictEqual( typeof ilazlr, 'function' );
});

test( 'ilazlr: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ilazlr.ndarray, 'function' );
});

test( 'ilazlr: diagonal 3x3 matrix -> last non-zero row = 2 (0-based)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlr_diag'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	buf[ 0 * 2 + 0 * 8 ] = 1.0; // (0,0) real
	buf[ 1 * 2 + 1 * 8 ] = 2.0; // (1,1) real
	buf[ 2 * 2 + 2 * 8 ] = 3.0; // (2,2) real
	var A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: last row all zeros -> returns 1 (0-based row 2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlr_row2'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	buf[ 0 * 2 + 0 * 8 ] = 1.0; // (0,0) real
	buf[ 1 * 2 + 1 * 8 ] = 2.0; // (1,1) real
	var A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: all zeros -> returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlr_zeros'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	var A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: M=0 -> returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlr_m_zero'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	var A = c128( buf );
	assert.strictEqual( base( 0, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: only imaginary part non-zero in last row', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlr_imag'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	// a(3, 2) = (0, 5i) -> row 2 (0-based), col 1 (0-based)
	buf[ 2 * 2 + 1 * 8 + 1 ] = 5.0; // imaginary part
	var A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: full matrix -> returns M-1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlr_full'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	var vals = [
		[1,1], [2,2], [3,3],
		[4,4], [5,5], [6,6],
		[7,7], [8,8], [9,9]
	];
	var i, j, k;
	k = 0;
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			buf[ i * 2 + j * 8 ] = vals[ k ][ 0 ];
			buf[ i * 2 + j * 8 + 1 ] = vals[ k ][ 1 ];
			k++;
		}
	}
	var A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: 1x1 non-zero -> returns 0', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlr_1x1'; } );
	var buf = new Float64Array( 2 * 4 );
	buf[ 0 ] = 1.0;
	var A = c128( buf );
	assert.strictEqual( base( 1, 1, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlr: 1x1 zero -> returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlr_1x1_zero'; } );
	var buf = new Float64Array( 2 * 4 );
	var A = c128( buf );
	assert.strictEqual( base( 1, 1, A, 1, 4, 0 ), expected( tc ) );
});
