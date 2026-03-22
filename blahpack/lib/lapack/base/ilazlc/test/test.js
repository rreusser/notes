'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var ilazlc = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ilazlc.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// Fortran returns 1-based; JS returns 0-based. Convert:
function expected( tc ) {
	return tc.result - 1;
}

// For column-major complex matrix with LDA rows:
// strideA1 = 1 (each row = 1 complex element)
// strideA2 = LDA (each column = LDA complex elements)

// Helper: build Complex128Array from flat interleaved doubles
function c128( arr ) {
	return new Complex128Array( arr );
}

// TESTS //

test( 'ilazlc: main export is a function', function t() {
	assert.strictEqual( typeof ilazlc, 'function' );
});

test( 'ilazlc: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ilazlc.ndarray, 'function' );
});

test( 'ilazlc: diagonal 3x3 matrix -> last non-zero column = 2 (0-based)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_diag'; } );
	// LDA=4, 3x3 matrix. strideA1=1, strideA2=4
	var buf = new Float64Array( 2 * 4 * 3 ); // 4 rows * 3 cols * 2 (re,im)
	buf[ 0 * 2 + 0 * 8 ] = 1.0; // (0,0) real
	buf[ 1 * 2 + 1 * 8 ] = 2.0; // (1,1) real
	buf[ 2 * 2 + 2 * 8 ] = 3.0; // (2,2) real
	var A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: last column all zeros -> returns 1 (0-based col 2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_col2'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	buf[ 0 * 2 + 0 * 8 ] = 1.0; // (0,0) real
	buf[ 1 * 2 + 1 * 8 ] = 2.0; // (1,1) real
	var A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: all zeros -> returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_zeros'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	var A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: N=0 -> returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_n_zero'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	var A = c128( buf );
	assert.strictEqual( base( 3, 0, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: only imaginary part non-zero in last column', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_imag'; } );
	var buf = new Float64Array( 2 * 4 * 3 );
	// a(2, 3) = (0, 5i) -> row 1 (0-based), col 2 (0-based)
	buf[ 1 * 2 + 2 * 8 + 1 ] = 5.0; // imaginary part
	var A = c128( buf );
	assert.strictEqual( base( 3, 3, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: full matrix -> returns N-1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_full'; } );
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

test( 'ilazlc: 1x1 non-zero -> returns 0', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_1x1'; } );
	var buf = new Float64Array( 2 * 4 );
	buf[ 0 ] = 1.0;
	var A = c128( buf );
	assert.strictEqual( base( 1, 1, A, 1, 4, 0 ), expected( tc ) );
});

test( 'ilazlc: 1x1 zero -> returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_1x1_zero'; } );
	var buf = new Float64Array( 2 * 4 );
	var A = c128( buf );
	assert.strictEqual( base( 1, 1, A, 1, 4, 0 ), expected( tc ) );
});
