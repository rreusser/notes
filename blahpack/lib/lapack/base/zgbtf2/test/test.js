'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgbtf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgbtf2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch: actual ' + actual.length + ' vs expected ' + expected.length );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zgbtf2: main export is a function', function t() {
	assert.strictEqual( typeof zgbtf2, 'function' );
});

test( 'zgbtf2: 4x4 tridiagonal (KL=1, KU=1)', function t() {
	var tc = findCase( 'tridiag_4x4' );
	// LDAB=4, KL=1, KU=1, KV=KU+KL=2
	// 4 rows x 4 cols in complex band storage
	var AB = new Complex128Array( 4 * 4 );
	var ABv = reinterpret( AB, 0 );
	// Fill: column-major, AB(row, col) -> ABv[(row + col*4)*2]
	// KV=2, diagonal at row 2 (0-based)
	// Col 0: row2=(4,1), row3=(-1,0)
	ABv[ (2 + 0 * 4) * 2 ] = 4; ABv[ (2 + 0 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 0 * 4) * 2 ] = -1; ABv[ (3 + 0 * 4) * 2 + 1 ] = 0;
	// Col 1: row1=(-1,0), row2=(4,1), row3=(-1,0)
	ABv[ (1 + 1 * 4) * 2 ] = -1; ABv[ (1 + 1 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 1 * 4) * 2 ] = 4; ABv[ (2 + 1 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 1 * 4) * 2 ] = -1; ABv[ (3 + 1 * 4) * 2 + 1 ] = 0;
	// Col 2: row1=(-1,0), row2=(4,1), row3=(-1,0)
	ABv[ (1 + 2 * 4) * 2 ] = -1; ABv[ (1 + 2 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 2 * 4) * 2 ] = 4; ABv[ (2 + 2 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 2 * 4) * 2 ] = -1; ABv[ (3 + 2 * 4) * 2 + 1 ] = 0;
	// Col 3: row1=(-1,0), row2=(4,1)
	ABv[ (1 + 3 * 4) * 2 ] = -1; ABv[ (1 + 3 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 3 * 4) * 2 ] = 4; ABv[ (2 + 3 * 4) * 2 + 1 ] = 1;

	var IPIV = new Int32Array( 4 );
	var info = zgbtf2( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( ABv ), tc.AB, 1e-10, 'AB' );
	// Fortran IPIV is 1-based, JS is 0-based
	var i;
	for ( i = 0; i < tc.ipiv.length; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgbtf2: 3x3 with KL=1, KU=2', function t() {
	var tc = findCase( 'kl1_ku2_3x3' );
	// LDAB=5, KL=1, KU=2, KV=3
	var AB = new Complex128Array( 5 * 3 );
	var ABv = reinterpret( AB, 0 );
	// Col 0: row3=(5,1), row4=(2,0)
	ABv[ (3 + 0 * 5) * 2 ] = 5; ABv[ (3 + 0 * 5) * 2 + 1 ] = 1;
	ABv[ (4 + 0 * 5) * 2 ] = 2; ABv[ (4 + 0 * 5) * 2 + 1 ] = 0;
	// Col 1: row2=(3,0), row3=(6,1), row4=(1,0)
	ABv[ (2 + 1 * 5) * 2 ] = 3; ABv[ (2 + 1 * 5) * 2 + 1 ] = 0;
	ABv[ (3 + 1 * 5) * 2 ] = 6; ABv[ (3 + 1 * 5) * 2 + 1 ] = 1;
	ABv[ (4 + 1 * 5) * 2 ] = 1; ABv[ (4 + 1 * 5) * 2 + 1 ] = 0;
	// Col 2: row1=(1,1), row2=(4,0), row3=(7,1)
	ABv[ (1 + 2 * 5) * 2 ] = 1; ABv[ (1 + 2 * 5) * 2 + 1 ] = 1;
	ABv[ (2 + 2 * 5) * 2 ] = 4; ABv[ (2 + 2 * 5) * 2 + 1 ] = 0;
	ABv[ (3 + 2 * 5) * 2 ] = 7; ABv[ (3 + 2 * 5) * 2 + 1 ] = 1;

	var IPIV = new Int32Array( 3 );
	var info = zgbtf2( 3, 3, 1, 2, AB, 1, 5, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( ABv ), tc.AB, 1e-10, 'AB' );
	var i;
	for ( i = 0; i < tc.ipiv.length; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgbtf2: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var AB = new Complex128Array( 4 );
	var IPIV = new Int32Array( 1 );
	var info = zgbtf2( 3, 0, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'zgbtf2: M=0 quick return', function t() {
	var tc = findCase( 'm_zero' );
	var AB = new Complex128Array( 4 );
	var IPIV = new Int32Array( 1 );
	var info = zgbtf2( 0, 3, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'zgbtf2: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var AB = new Complex128Array( [ 7, 2 ] );
	var IPIV = new Int32Array( 1 );
	var info = zgbtf2( 1, 1, 0, 0, AB, 1, 1, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( reinterpret( AB, 0 ) ), tc.AB, 1e-10, 'AB' );
	assert.strictEqual( IPIV[ 0 ], tc.ipiv[ 0 ] - 1, 'ipiv[0]' );
});

test( 'zgbtf2: singular matrix', function t() {
	var tc = findCase( 'singular' );
	// 2x2, KL=0, KU=1, LDAB=2
	var AB = new Complex128Array( 2 * 2 );
	var ABv = reinterpret( AB, 0 );
	// Col 0: row0=(0,0), row1=(0,0)
	// Col 1: row0=(1,1), row1=(0,0)
	ABv[ (0 + 1 * 2) * 2 ] = 1; ABv[ (0 + 1 * 2) * 2 + 1 ] = 1;
	var IPIV = new Int32Array( 2 );
	var info = zgbtf2( 2, 2, 0, 1, AB, 1, 2, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'zgbtf2: 5x3 tall matrix (M > N)', function t() {
	var tc = findCase( 'tall_5x3' );
	// LDAB=4, KL=1, KU=1, KV=2
	var AB = new Complex128Array( 4 * 3 );
	var ABv = reinterpret( AB, 0 );
	// Col 0: row2=(4,1), row3=(-1,0)
	ABv[ (2 + 0 * 4) * 2 ] = 4; ABv[ (2 + 0 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 0 * 4) * 2 ] = -1; ABv[ (3 + 0 * 4) * 2 + 1 ] = 0;
	// Col 1: row1=(-1,0), row2=(4,1), row3=(-1,0)
	ABv[ (1 + 1 * 4) * 2 ] = -1; ABv[ (1 + 1 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 1 * 4) * 2 ] = 4; ABv[ (2 + 1 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 1 * 4) * 2 ] = -1; ABv[ (3 + 1 * 4) * 2 + 1 ] = 0;
	// Col 2: row1=(-1,0), row2=(4,1), row3=(-1,0)
	ABv[ (1 + 2 * 4) * 2 ] = -1; ABv[ (1 + 2 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 2 * 4) * 2 ] = 4; ABv[ (2 + 2 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 2 * 4) * 2 ] = -1; ABv[ (3 + 2 * 4) * 2 + 1 ] = 0;

	var IPIV = new Int32Array( 3 );
	var info = zgbtf2( 5, 3, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( ABv ), tc.AB, 1e-10, 'AB' );
	var i;
	for ( i = 0; i < tc.ipiv.length; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgbtf2: pivoting 2x2', function t() {
	var tc = findCase( 'pivot_2x2' );
	// LDAB=4, KL=1, KU=1
	// Full A = [1+0i  2+0i; 3+0i  4+0i]
	var AB = new Complex128Array( 4 * 2 );
	var ABv = reinterpret( AB, 0 );
	// Col 0: row2=(1,0), row3=(3,0)
	ABv[ (2 + 0 * 4) * 2 ] = 1; ABv[ (2 + 0 * 4) * 2 + 1 ] = 0;
	ABv[ (3 + 0 * 4) * 2 ] = 3; ABv[ (3 + 0 * 4) * 2 + 1 ] = 0;
	// Col 1: row1=(2,0), row2=(4,0)
	ABv[ (1 + 1 * 4) * 2 ] = 2; ABv[ (1 + 1 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 1 * 4) * 2 ] = 4; ABv[ (2 + 1 * 4) * 2 + 1 ] = 0;

	var IPIV = new Int32Array( 2 );
	var info = zgbtf2( 2, 2, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( ABv ), tc.AB, 1e-10, 'AB' );
	var i;
	for ( i = 0; i < tc.ipiv.length; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});
