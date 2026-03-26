'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgetc2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgetc2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dgetc2: basic 2x2', function t() {
	var tc = findCase( 'basic_2x2' );
	// Column-major 2x2: [A(1,1), A(2,1), A(1,2), A(2,2)]
	var A = new Float64Array( [ 1.0, 3.0, 2.0, 4.0 ] );
	var IPIV = new Int32Array( 2 );
	var JPIV = new Int32Array( 2 );
	var info = dgetc2( 2, A, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	// Fortran IPIV/JPIV are 1-based, JS are 0-based
	var i;
	for ( i = 0; i < 2; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
		assert.strictEqual( JPIV[ i ], tc.jpiv[ i ] - 1, 'jpiv[' + i + ']' );
	}
});

test( 'dgetc2: basic 3x3', function t() {
	var tc = findCase( 'basic_3x3' );
	// Column-major 3x3, stored in 3-element stride columns
	var A = new Float64Array( [
		1.0, 4.0, 7.0,
		2.0, 5.0, 8.0,
		3.0, 6.0, 10.0
	] );
	var IPIV = new Int32Array( 3 );
	var JPIV = new Int32Array( 3 );
	var info = dgetc2( 3, A, 1, 3, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	var i;
	for ( i = 0; i < 3; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
		assert.strictEqual( JPIV[ i ], tc.jpiv[ i ] - 1, 'jpiv[' + i + ']' );
	}
});

test( 'dgetc2: basic 4x4 with complete pivoting', function t() {
	var tc = findCase( 'basic_4x4' );
	// Column-major 4x4
	var A = new Float64Array( [
		0.1, 0.4, 0.8, 1.2,
		0.2, 0.5, 0.9, 1.3,
		0.3, 0.6, 1.0, 1.4,
		10.0, 0.7, 1.1, 1.5
	] );
	var IPIV = new Int32Array( 4 );
	var JPIV = new Int32Array( 4 );
	var info = dgetc2( 4, A, 1, 4, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	var i;
	for ( i = 0; i < 4; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
		assert.strictEqual( JPIV[ i ], tc.jpiv[ i ] - 1, 'jpiv[' + i + ']' );
	}
});

test( 'dgetc2: N=1', function t() {
	var tc = findCase( 'n_equals_1' );
	var A = new Float64Array( [ 5.0 ] );
	var IPIV = new Int32Array( 1 );
	var JPIV = new Int32Array( 1 );
	var info = dgetc2( 1, A, 1, 1, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	assert.strictEqual( IPIV[ 0 ], tc.ipiv[ 0 ] - 1, 'ipiv[0]' );
	assert.strictEqual( JPIV[ 0 ], tc.jpiv[ 0 ] - 1, 'jpiv[0]' );
});

test( 'dgetc2: near-singular', function t() {
	var tc = findCase( 'near_singular' );
	var A = new Float64Array( [ 1e-200, 1.0, 1.0, 1.0 ] );
	var IPIV = new Int32Array( 2 );
	var JPIV = new Int32Array( 2 );
	var info = dgetc2( 2, A, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	var i;
	for ( i = 0; i < 2; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
		assert.strictEqual( JPIV[ i ], tc.jpiv[ i ] - 1, 'jpiv[' + i + ']' );
	}
});

test( 'dgetc2: N=0', function t() {
	var A = new Float64Array( 1 );
	var IPIV = new Int32Array( 1 );
	var JPIV = new Int32Array( 1 );
	var info = dgetc2( 0, A, 1, 1, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, 0, 'info' );
});
