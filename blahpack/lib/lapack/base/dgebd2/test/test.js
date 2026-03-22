

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dgebd2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgebd2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgebd2: 4x3 upper bidiagonal (M > N)', function t() {
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var tc;
	var A;
	var D;
	var E;

	tc = findCase( '4x3_upper' );

	// 4x3 matrix, column-major: strideA1=1, strideA2=4
	A = new Float64Array([
		2.0, 1.0, 3.0, 1.0,  // col 0
		1.0, 4.0, 2.0, 3.0,  // col 1
		3.0, 2.0, 5.0, 1.0   // col 2
	]);
	D = new Float64Array( 3 );
	E = new Float64Array( 2 );
	TAUQ = new Float64Array( 3 );
	TAUP = new Float64Array( 3 );
	WORK = new Float64Array( 4 );

	info = dgebd2( 4, 3, A, 1, 4, 0, D, 1, 0, E, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( E ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});

test( 'dgebd2: 3x4 lower bidiagonal (M < N)', function t() {
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var tc;
	var A;
	var D;
	var E;

	tc = findCase( '3x4_lower' );

	// 3x4 matrix, column-major: strideA1=1, strideA2=3
	A = new Float64Array([
		2.0, 4.0, 1.0,  // col 0
		1.0, 2.0, 5.0,  // col 1
		3.0, 1.0, 2.0,  // col 2
		1.0, 3.0, 4.0   // col 3
	]);
	D = new Float64Array( 3 );
	E = new Float64Array( 2 );
	TAUQ = new Float64Array( 3 );
	TAUP = new Float64Array( 3 );
	WORK = new Float64Array( 4 );

	info = dgebd2( 3, 4, A, 1, 3, 0, D, 1, 0, E, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( E ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});

test( 'dgebd2: 3x3 square matrix', function t() {
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var tc;
	var A;
	var D;
	var E;

	tc = findCase( '3x3_square' );

	// 3x3 matrix, column-major: strideA1=1, strideA2=3
	A = new Float64Array([
		5.0, 3.0, 1.0,  // col 0
		2.0, 4.0, 3.0,  // col 1
		1.0, 2.0, 6.0   // col 2
	]);
	D = new Float64Array( 3 );
	E = new Float64Array( 2 );
	TAUQ = new Float64Array( 3 );
	TAUP = new Float64Array( 3 );
	WORK = new Float64Array( 3 );

	info = dgebd2( 3, 3, A, 1, 3, 0, D, 1, 0, E, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( E ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});

test( 'dgebd2: 1x3 (M=1, M < N)', function t() {
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var tc;
	var A;
	var D;

	tc = findCase( '1x3' );

	// 1x3 matrix, column-major: strideA1=1, strideA2=1
	A = new Float64Array([
		2.0, 3.0, 4.0
	]);
	D = new Float64Array( 1 );
	TAUQ = new Float64Array( 1 );
	TAUP = new Float64Array( 1 );
	WORK = new Float64Array( 3 );

	info = dgebd2( 1, 3, A, 1, 1, 0, D, 1, 0, new Float64Array( 0 ), 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});

test( 'dgebd2: 3x1 (N=1, M > N)', function t() {
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var tc;
	var A;
	var D;

	tc = findCase( '3x1' );

	// 3x1 matrix, column-major: strideA1=1, strideA2=3
	A = new Float64Array([
		2.0, 3.0, 4.0
	]);
	D = new Float64Array( 1 );
	TAUQ = new Float64Array( 1 );
	TAUP = new Float64Array( 1 );
	WORK = new Float64Array( 3 );

	info = dgebd2( 3, 1, A, 1, 3, 0, D, 1, 0, new Float64Array( 0 ), 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});

test( 'dgebd2: M=0 quick return', function t() {
	var info = dgebd2( 0, 3, new Float64Array( 0 ), 1, 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0 );
	assert.equal( info, 0 );
});

test( 'dgebd2: N=0 quick return', function t() {
	var info = dgebd2( 3, 0, new Float64Array( 0 ), 1, 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0 );
	assert.equal( info, 0 );
});

test( 'dgebd2: 1x1 matrix', function t() {
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var tc;
	var A;
	var D;

	tc = findCase( '1x1' );

	A = new Float64Array([ 7.0 ]);
	D = new Float64Array( 1 );
	TAUQ = new Float64Array( 1 );
	TAUP = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dgebd2( 1, 1, A, 1, 1, 0, D, 1, 0, new Float64Array( 0 ), 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});
