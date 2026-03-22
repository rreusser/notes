

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlabrd = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlabrd.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zlabrd: m_ge_n_5x4_nb2', function t() {
	var tc = findCase( 'm_ge_n_5x4_nb2' );
	var M = 5;
	var N = 4;
	var nb = 2;
	var LDA = M;
	var LDX = M;
	var LDY = N;

	// A is 5x4 column-major, interleaved complex => 2*5*4 = 40 Float64s
	var A = new Float64Array([
		1.0, 0.5, 2.0, -1.0, -0.5, 0.3, 0.7, -0.2, 1.5, 0.8,    // col 0
		0.3, 0.4, -1.0, 0.5, 0.6, -0.7, 1.2, 0.1, -0.3, 0.9,    // col 1
		0.5, -0.1, 0.8, 0.2, -0.4, 1.0, 0.2, -0.5, 1.1, 0.3,    // col 2
		-0.2, 0.6, 0.4, -0.3, 0.9, 0.1, -0.6, 0.8, 0.3, -0.4    // col 3
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( 2 * nb );
	var TAUP = new Float64Array( 2 * nb );
	var X = new Float64Array( 2 * LDX * nb );
	var Y = new Float64Array( 2 * LDY * nb );

	zlabrd( M, N, nb, A, 1, LDA, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, LDX, 0, Y, 1, LDY, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-12, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-12, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-12, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-12, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-12, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-12, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-12, 'Y' );
});

test( 'zlabrd: m_lt_n_4x5_nb2', function t() {
	var tc = findCase( 'm_lt_n_4x5_nb2' );
	var M = 4;
	var N = 5;
	var nb = 2;
	var LDA = M;
	var LDX = M;
	var LDY = N;

	// A is 4x5 column-major, interleaved complex => 2*4*5 = 40 Float64s
	var A = new Float64Array([
		1.0, 0.5, 2.0, -1.0, -0.5, 0.3, 0.7, -0.2,              // col 0
		0.3, 0.4, -1.0, 0.5, 0.6, -0.7, 1.2, 0.1,               // col 1
		0.5, -0.1, 0.8, 0.2, -0.4, 1.0, 0.2, -0.5,              // col 2
		-0.2, 0.6, 0.4, -0.3, 0.9, 0.1, -0.6, 0.8,              // col 3
		1.5, 0.8, -0.3, 0.9, 1.1, 0.3, 0.3, -0.4                // col 4
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( 2 * nb );
	var TAUP = new Float64Array( 2 * nb );
	var X = new Float64Array( 2 * LDX * nb );
	var Y = new Float64Array( 2 * LDY * nb );

	zlabrd( M, N, nb, A, 1, LDA, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, LDX, 0, Y, 1, LDY, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-12, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-12, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-12, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-12, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-12, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-12, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-12, 'Y' );
});

test( 'zlabrd: quick_return_m0', function t() {
	var tc = findCase( 'quick_return_m0' );
	var d = new Float64Array( 2 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 4 );
	var TAUP = new Float64Array( 4 );
	var A = new Float64Array( 8 );
	var X = new Float64Array( 4 );
	var Y = new Float64Array( 16 );

	zlabrd( 0, 4, 2, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, 1, 0, Y, 1, 4, 0 );

	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
});

test( 'zlabrd: nb1_3x3', function t() {
	var tc = findCase( 'nb1_3x3' );
	var M = 3;
	var N = 3;
	var nb = 1;
	var LDA = M;
	var LDX = M;
	var LDY = N;

	// A is 3x3 column-major, interleaved complex => 2*3*3 = 18 Float64s
	var A = new Float64Array([
		2.0, 1.0, -1.0, 0.5, 0.3, -0.2,       // col 0
		0.5, -0.4, 1.0, 0.3, -0.7, 0.6,        // col 1
		0.8, 0.2, -0.3, -0.1, 1.5, -0.5         // col 2
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( 2 * nb );
	var TAUP = new Float64Array( 2 * nb );
	var X = new Float64Array( 2 * LDX * nb );
	var Y = new Float64Array( 2 * LDY * nb );

	zlabrd( M, N, nb, A, 1, LDA, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, LDX, 0, Y, 1, LDY, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-12, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-12, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-12, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-12, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-12, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-12, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-12, 'Y' );
});

test( 'zlabrd: nb1_m_lt_n_2x3', function t() {
	var tc = findCase( 'nb1_m_lt_n_2x3' );
	var M = 2;
	var N = 3;
	var nb = 1;
	var LDA = M;
	var LDX = M;
	var LDY = N;

	// A is 2x3 column-major, interleaved complex => 2*2*3 = 12 Float64s
	var A = new Float64Array([
		1.5, 0.5, -0.8, 0.3,                    // col 0
		0.6, -0.2, 1.0, 0.7,                    // col 1
		-0.4, 0.9, 0.2, -0.6                    // col 2
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( 2 * nb );
	var TAUP = new Float64Array( 2 * nb );
	var X = new Float64Array( 2 * LDX * nb );
	var Y = new Float64Array( 2 * LDY * nb );

	zlabrd( M, N, nb, A, 1, LDA, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, LDX, 0, Y, 1, LDY, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-12, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-12, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-12, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-12, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-12, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-12, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-12, 'Y' );
});

test( 'zlabrd: m_lt_n_nb_eq_m_2x4', function t() {
	var tc = findCase( 'm_lt_n_nb_eq_m_2x4' );
	var M = 2;
	var N = 4;
	var nb = 2;
	var LDA = M;
	var LDX = M;
	var LDY = N;

	// A is 2x4 column-major, interleaved complex => 2*2*4 = 16 Float64s
	var A = new Float64Array([
		1.5, 0.5, -0.8, 0.3,                    // col 0
		0.6, -0.2, 1.0, 0.7,                    // col 1
		-0.4, 0.9, 0.2, -0.6,                   // col 2
		0.7, -0.1, -0.3, 0.4                    // col 3
	]);
	var d = new Float64Array( nb );
	var e = new Float64Array( nb );
	var TAUQ = new Float64Array( 2 * nb );
	var TAUP = new Float64Array( 2 * nb );
	var X = new Float64Array( 2 * LDX * nb );
	var Y = new Float64Array( 2 * LDY * nb );

	zlabrd( M, N, nb, A, 1, LDA, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, X, 1, LDX, 0, Y, 1, LDY, 0 );

	assertArrayClose( Array.from( A ), tc.A, 1e-12, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-12, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-12, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-12, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-12, 'TAUP' );
	assertArrayClose( Array.from( X ), tc.X, 1e-12, 'X' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-12, 'Y' );
});
