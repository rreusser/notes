'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgbsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgbsv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Creates a complex band matrix in column-major layout.
*
* @param {number} ldab - leading dimension (2*KL+KU+1) in complex elements
* @param {number} n - number of columns
* @param {Array} entries - array of [row, col, re, im] (0-based row/col in band storage)
* @returns {Complex128Array} band matrix
*/
function bandMatrix( ldab, n, entries ) {
	var ABv;
	var AB;
	var i;
	AB = new Complex128Array( ldab * n );
	ABv = reinterpret( AB, 0 );
	for ( i = 0; i < entries.length; i++ ) {
		ABv[ 2 * ( entries[ i ][ 0 ] + entries[ i ][ 1 ] * ldab ) ] = entries[ i ][ 2 ];
		ABv[ 2 * ( entries[ i ][ 0 ] + entries[ i ][ 1 ] * ldab ) + 1 ] = entries[ i ][ 3 ];
	}
	return AB;
}


// TESTS //

test( 'zgbsv: 4x4 complex tridiagonal, single RHS', function t() {
	var tc = findCase( 'tridiag_4x4_1rhs' );
	var IPIV;
	var info;
	var AB;
	var Bv;
	var B;
	var i;

	// N=4, KL=1, KU=1, LDAB=2*1+1+1=4
	// Band storage (0-indexed rows): row0=fill, row1=superdiag, row2=diag, row3=subdiag
	AB = bandMatrix( 4, 4, [
		// col 0
		[ 2, 0, 4.0, 1.0 ], [ 3, 0, -1.0, 0.0 ],
		// col 1
		[ 1, 1, -1.0, 0.0 ], [ 2, 1, 4.0, 1.0 ], [ 3, 1, -1.0, 0.0 ],
		// col 2
		[ 1, 2, -1.0, 0.0 ], [ 2, 2, 4.0, 1.0 ], [ 3, 2, -1.0, 0.0 ],
		// col 3
		[ 1, 3, -1.0, 0.0 ], [ 2, 3, 4.0, 1.0 ]
	]);

	// b = [1, 2, 3, 4] (real-valued)
	B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
	IPIV = new Int32Array( 4 );

	info = zgbsv( 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	Bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( Bv ).slice( 0, 8 ), tc.x, 1e-12, 'x' );

	// Verify IPIV (fixture is 1-based Fortran, JS is 0-based)
	for ( i = 0; i < 4; i++ ) {
		assert.equal( IPIV[ i ], tc.ipiv[ i ] - 1, 'IPIV[' + i + ']' );
	}
});

test( 'zgbsv: 4x4 complex tridiagonal, 2 RHS', function t() {
	var tc = findCase( 'tridiag_4x4_2rhs' );
	var IPIV;
	var info;
	var AB;
	var Bv;
	var B;

	AB = bandMatrix( 4, 4, [
		[ 2, 0, 4.0, 1.0 ], [ 3, 0, -1.0, 0.0 ],
		[ 1, 1, -1.0, 0.0 ], [ 2, 1, 4.0, 1.0 ], [ 3, 1, -1.0, 0.0 ],
		[ 1, 2, -1.0, 0.0 ], [ 2, 2, 4.0, 1.0 ], [ 3, 2, -1.0, 0.0 ],
		[ 1, 3, -1.0, 0.0 ], [ 2, 3, 4.0, 1.0 ]
	]);

	// B: N=4, NRHS=2, LDB=4 (tightly packed)
	B = new Complex128Array( 8 );
	Bv = reinterpret( B, 0 );
	// RHS 1: [1, 2, 3, 4]
	Bv[ 0 ] = 1.0; Bv[ 1 ] = 0.0;
	Bv[ 2 ] = 2.0; Bv[ 3 ] = 0.0;
	Bv[ 4 ] = 3.0; Bv[ 5 ] = 0.0;
	Bv[ 6 ] = 4.0; Bv[ 7 ] = 0.0;
	// RHS 2: [5+1i, 6+2i, 7+3i, 8+4i]
	Bv[ 8 ] = 5.0; Bv[ 9 ] = 1.0;
	Bv[ 10 ] = 6.0; Bv[ 11 ] = 2.0;
	Bv[ 12 ] = 7.0; Bv[ 13 ] = 3.0;
	Bv[ 14 ] = 8.0; Bv[ 15 ] = 4.0;

	IPIV = new Int32Array( 4 );

	info = zgbsv( 4, 1, 1, 2, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	Bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( Bv ).slice( 0, 16 ), tc.x, 1e-12, 'x' );
});

test( 'zgbsv: N=1 scalar system', function t() {
	var tc = findCase( 'n_one' );
	var IPIV;
	var info;
	var AB;
	var Bv;
	var B;

	// A = [3+2i], KL=0, KU=0, LDAB=1
	AB = new Complex128Array( [ 3.0, 2.0 ] );
	// b = [6+4i], expected x = [2+0i]
	B = new Complex128Array( [ 6.0, 4.0 ] );
	IPIV = new Int32Array( 1 );

	info = zgbsv( 1, 0, 0, 1, AB, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	Bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( Bv ).slice( 0, 2 ), tc.x, 1e-12, 'x' );
});

test( 'zgbsv: singular matrix returns info > 0', function t() {
	var tc = findCase( 'singular' );
	var IPIV;
	var info;
	var AB;
	var B;

	// 2x2 diagonal: A = [1+0i, 0; 0, 0+0i], KL=1, KU=1, LDAB=4
	AB = bandMatrix( 4, 2, [
		[ 2, 0, 1.0, 0.0 ]  // diag(0)=1, diag(1)=0 (default)
	]);
	B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
	IPIV = new Int32Array( 2 );

	info = zgbsv( 2, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 2, 0 );

	assert.equal( info, tc.info, 'info (singular at position 2)' );
});

test( 'zgbsv: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var IPIV;
	var info;
	var AB;
	var B;

	AB = new Complex128Array( 4 );
	B = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );

	info = zgbsv( 0, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'zgbsv: complex RHS with imaginary parts', function t() {
	var tc = findCase( 'complex_rhs' );
	var IPIV;
	var info;
	var AB;
	var Bv;
	var B;

	AB = bandMatrix( 4, 4, [
		[ 2, 0, 4.0, 1.0 ], [ 3, 0, -1.0, 0.0 ],
		[ 1, 1, -1.0, 0.0 ], [ 2, 1, 4.0, 1.0 ], [ 3, 1, -1.0, 0.0 ],
		[ 1, 2, -1.0, 0.0 ], [ 2, 2, 4.0, 1.0 ], [ 3, 2, -1.0, 0.0 ],
		[ 1, 3, -1.0, 0.0 ], [ 2, 3, 4.0, 1.0 ]
	]);

	// b = [1+2i, 3+4i, 5+6i, 7+8i]
	B = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
	IPIV = new Int32Array( 4 );

	info = zgbsv( 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	Bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( Bv ).slice( 0, 8 ), tc.x, 1e-12, 'x' );
});
