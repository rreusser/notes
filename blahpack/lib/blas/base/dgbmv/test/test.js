

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgbmv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgbmv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// Band storage for a 4x5 matrix with KL=1, KU=2, stored in (KL+KU+1)x5 = 4x5 band format.
// Full matrix A (4x5):
//   [ 1  2  3  0  0 ]
//   [ 4  5  6  7  0 ]
//   [ 0  8  9 10 11 ]
//   [ 0  0 12 13 14 ]
//
// Band storage (column-major, 4 rows per column, diagonal at row KU=2):
// col 0: row2=1, row3=4
// col 1: row1=2, row2=5, row3=8
// col 2: row0=3, row1=6, row2=9, row3=12
// col 3: row0=7, row1=10, row2=13
// col 4: row0=11, row1=14

function bandA() {
	var a = new Float64Array( 20 );

	// Column-major: a[ row + col*4 ]
	// col 0
	a[ 2 + 0 * 4 ] = 1.0;
	a[ 3 + 0 * 4 ] = 4.0;

	// col 1
	a[ 1 + 1 * 4 ] = 2.0;
	a[ 2 + 1 * 4 ] = 5.0;
	a[ 3 + 1 * 4 ] = 8.0;

	// col 2
	a[ 0 + 2 * 4 ] = 3.0;
	a[ 1 + 2 * 4 ] = 6.0;
	a[ 2 + 2 * 4 ] = 9.0;
	a[ 3 + 2 * 4 ] = 12.0;

	// col 3
	a[ 0 + 3 * 4 ] = 7.0;
	a[ 1 + 3 * 4 ] = 10.0;
	a[ 2 + 3 * 4 ] = 13.0;

	// col 4
	a[ 0 + 4 * 4 ] = 11.0;
	a[ 1 + 4 * 4 ] = 14.0;

	return a;
}

test( 'dgbmv: notrans', function t() {
	var tc = findCase( 'notrans' );
	var a = bandA();
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( 4 );

	dgbmv( 'no-transpose', 4, 5, 1, 2, 1.0, a, 1, 4, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dgbmv: trans', function t() {
	var tc = findCase( 'trans' );
	var a = bandA();
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var y = new Float64Array( 5 );

	dgbmv( 'transpose', 4, 5, 1, 2, 1.0, a, 1, 4, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dgbmv: alpha_beta', function t() {
	var tc = findCase( 'alpha_beta' );
	var a = bandA();
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 10.0, 20.0, 30.0, 40.0 ] );

	dgbmv( 'no-transpose', 4, 5, 1, 2, 2.0, a, 1, 4, 0, x, 1, 0, 0.5, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dgbmv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var y = new Float64Array( [ 99.0 ] );
	var a = new Float64Array( 1 );
	var x = new Float64Array( 1 );

	dgbmv( 'no-transpose', 0, 0, 0, 0, 1.0, a, 1, 1, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dgbmv: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	var a = bandA();
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );

	dgbmv( 'no-transpose', 4, 5, 1, 2, 0.0, a, 1, 4, 0, x, 1, 0, 2.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dgbmv: stride', function t() {
	var tc = findCase( 'stride' );
	var a = bandA();
	var x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0, 5.0, 0.0 ] );
	var y = new Float64Array( 8 );

	dgbmv( 'no-transpose', 4, 5, 1, 2, 1.0, a, 1, 4, 0, x, 2, 0, 0.0, y, 2, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});
