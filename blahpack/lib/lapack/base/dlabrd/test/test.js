'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlabrd = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlabrd.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlabrd: m_ge_n_6x5_nb3 (upper bidiagonal)', function t() {
	var tc = findCase( 'm_ge_n_6x5_nb3' );
	var M = 6;
	var N = 5;
	var nb = 3;

	// Column-major: A is M-by-N, strideA1=1, strideA2=M
	var A = new Float64Array([
		 1.0,  2.0, -0.5,  0.7,  1.5, -0.3,
		 0.3, -1.0,  0.6,  1.2, -0.3,  0.4,
		 0.5,  0.8, -0.4,  0.2,  1.1, -0.6,
		-0.2,  0.4,  0.9, -0.6,  0.3,  0.7,
		 0.8, -0.1,  0.2,  1.3, -0.5,  0.9
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( nb );
	var TAUP = new Float64Array( nb );
	var X = new Float64Array( M * nb );
	var Y = new Float64Array( N * nb );

	dlabrd( M, N, nb, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, M, 0, Y, 1, N, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-14, 'Y' );
});

test( 'dlabrd: m_lt_n_5x6_nb3 (lower bidiagonal)', function t() {
	var tc = findCase( 'm_lt_n_5x6_nb3' );
	var M = 5;
	var N = 6;
	var nb = 3;

	var A = new Float64Array([
		 1.0,  2.0, -0.5,  0.7,  1.5,
		 0.3, -1.0,  0.6,  1.2, -0.3,
		 0.5,  0.8, -0.4,  0.2,  1.1,
		-0.2,  0.4,  0.9, -0.6,  0.3,
		 0.8, -0.1,  0.2,  1.3, -0.5,
		-0.3,  0.4,  0.7,  0.9, -0.6
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( nb );
	var TAUP = new Float64Array( nb );
	var X = new Float64Array( M * nb );
	var Y = new Float64Array( N * nb );

	dlabrd( M, N, nb, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, M, 0, Y, 1, N, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-14, 'Y' );
});

test( 'dlabrd: square_4x4_nb2 (M=N, upper bidiagonal)', function t() {
	var tc = findCase( 'square_4x4_nb2' );
	var M = 4;
	var N = 4;
	var nb = 2;

	var A = new Float64Array([
		 2.0, -1.0,  0.3,  0.5,
		 0.5,  1.0, -0.7,  0.4,
		 0.8, -0.3,  1.5, -0.2,
		-0.4,  0.6,  0.1,  0.9
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( nb );
	var TAUP = new Float64Array( nb );
	var X = new Float64Array( M * nb );
	var Y = new Float64Array( N * nb );

	dlabrd( M, N, nb, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, M, 0, Y, 1, N, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-14, 'Y' );
});

test( 'dlabrd: nb0_quick_return', function t() {
	var tc = findCase( 'nb0_quick_return' );

	// NB=0: quick return, A should be unchanged
	var A = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var d = new Float64Array( 2 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 2 );
	var TAUP = new Float64Array( 2 );
	var X = new Float64Array( 4 );
	var Y = new Float64Array( 4 );

	dlabrd( 2, 2, 0, A, 1, 2, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, 2, 0, Y, 1, 2, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dlabrd: nb1_3x3 (single step, M >= N)', function t() {
	var tc = findCase( 'nb1_3x3' );
	var M = 3;
	var N = 3;
	var nb = 1;

	var A = new Float64Array([
		 2.0, -1.0,  0.3,
		 0.5,  1.0, -0.7,
		 0.8, -0.3,  1.5
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( nb );
	var TAUP = new Float64Array( nb );
	var X = new Float64Array( M * nb );
	var Y = new Float64Array( N * nb );

	dlabrd( M, N, nb, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, M, 0, Y, 1, N, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-14, 'Y' );
});

test( 'dlabrd: nb1_m_lt_n_2x3 (single step, M < N)', function t() {
	var tc = findCase( 'nb1_m_lt_n_2x3' );
	var M = 2;
	var N = 3;
	var nb = 1;

	var A = new Float64Array([
		 1.5, -0.8,
		 0.6,  1.0,
		-0.4,  0.2
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( nb );
	var TAUP = new Float64Array( nb );
	var X = new Float64Array( M * nb );
	var Y = new Float64Array( N * nb );

	dlabrd( M, N, nb, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, M, 0, Y, 1, N, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-14, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-14, 'Y' );
});

test( 'dlabrd: quick return when M=0', function t() {
	var A = new Float64Array([ 1.0, 2.0 ]);
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 1 );
	var TAUP = new Float64Array( 1 );
	var X = new Float64Array( 2 );
	var Y = new Float64Array( 2 );

	dlabrd( 0, 2, 1, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, 1, 0, Y, 1, 2, 0 );

	// A should be unchanged
	assert.equal( A[ 0 ], 1.0 );
	assert.equal( A[ 1 ], 2.0 );
});

test( 'dlabrd: quick return when N=0', function t() {
	var A = new Float64Array([ 1.0, 2.0 ]);
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 1 );
	var TAUP = new Float64Array( 1 );
	var X = new Float64Array( 2 );
	var Y = new Float64Array( 2 );

	dlabrd( 2, 0, 1, A, 1, 2, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, 2, 0, Y, 1, 1, 0 );

	// A should be unchanged
	assert.equal( A[ 0 ], 1.0 );
	assert.equal( A[ 1 ], 2.0 );
});
