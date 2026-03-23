/* eslint-disable max-len */
'use strict';
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlahr2 = require( './../lib/base.js' );
var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlahr2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );
function findCase( name ) { return fixture.find( function find( t ) { return t.name === name; } ); }
function assertArrayClose( actual, expected, tol, msg ) {
	var diff, i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		diff = Math.abs( actual[ i ] - expected[ i ] );
		if ( diff > tol && diff / Math.max( Math.abs( expected[ i ] ), 1.0 ) > tol ) {
			assert.fail( msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		}
	}
}

test( 'dlahr2: main export is a function', function t() {
	assert.strictEqual( typeof dlahr2, 'function' );
});

test( 'dlahr2: basic 6x6, K=1, NB=2', function () {
	var tc = findCase( 'basic_6x6_k1_nb2' );
	var N = 6; var K = 1; var NB = 2;
	var LDA = N; var LDT = NB; var LDY = N;
	var A = new Float64Array([
		2, 1, 3, 1, 4, 2,
		1, 4, 1, 2, 1, 3,
		3, 1, 5, 1, 2, 1,
		1, 2, 1, 6, 1, 2,
		4, 1, 2, 1, 7, 1,
		2, 3, 1, 2, 1, 8
	]);
	var tau = new Float64Array( NB );
	var T = new Float64Array( LDT * NB );
	var Y = new Float64Array( LDY * NB );
	dlahr2( N, K, NB, A, 1, LDA, 0, tau, 1, 0, T, 1, 0, LDT, Y, 1, 0, LDY );
	assertArrayClose( Array.from( A ), tc.A, 1e-13, 'A' );
	assertArrayClose( Array.from( tau ), tc.TAU, 1e-13, 'TAU' );
	assertArrayClose( Array.from( T ), tc.T, 1e-13, 'T' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-13, 'Y' );
});

test( 'dlahr2: 8x8, K=1, NB=3', function () {
	var tc = findCase( '8x8_k1_nb3' );
	var N = 8; var K = 1; var NB = 3;
	var LDA = N; var LDT = NB; var LDY = N;
	var A = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		A[ i * N + i ] = i + 2;
	}
	A[1]  = 1.0; A[2]  = 2.0; A[3]  = 0.5;
	A[8]  = 1.5; A[9]  = 1.0; A[10] = 0.5;
	A[16] = 2.0; A[17] = 0.5; A[18] = 1.0;
	A[24] = 1.0; A[25] = 0.5; A[26] = 2.0;
	A[32] = 0.5; A[33] = 1.0; A[34] = 0.5;
	A[40] = 2.0; A[41] = 0.5; A[42] = 1.0;
	A[48] = 1.0; A[49] = 2.0; A[50] = 0.5;
	A[4]  = 1.0; A[5]  = 0.5; A[6]  = 2.0; A[7]  = 1.0;
	var tau = new Float64Array( NB );
	var T = new Float64Array( LDT * NB );
	var Y = new Float64Array( LDY * NB );
	dlahr2( N, K, NB, A, 1, LDA, 0, tau, 1, 0, T, 1, 0, LDT, Y, 1, 0, LDY );
	assertArrayClose( Array.from( A ), tc.A, 1e-13, 'A' );
	assertArrayClose( Array.from( tau ), tc.TAU, 1e-13, 'TAU' );
	assertArrayClose( Array.from( T ), tc.T, 1e-13, 'T' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-13, 'Y' );
});

test( 'dlahr2: 5x5, K=1, NB=1 (single column)', function () {
	var tc = findCase( '5x5_k1_nb1' );
	var N = 5; var K = 1; var NB = 1;
	var LDA = N; var LDT = NB; var LDY = N;
	var A = new Float64Array([
		1, 6, 11, 16, 21,
		2, 7, 12, 17, 22,
		3, 8, 13, 18, 23,
		4, 9, 14, 19, 24,
		5, 10, 15, 20, 25
	]);
	var tau = new Float64Array( NB );
	var T = new Float64Array( LDT * NB );
	var Y = new Float64Array( LDY * NB );
	dlahr2( N, K, NB, A, 1, LDA, 0, tau, 1, 0, T, 1, 0, LDT, Y, 1, 0, LDY );
	assertArrayClose( Array.from( A ), tc.A, 1e-13, 'A' );
	assertArrayClose( Array.from( tau ), tc.TAU, 1e-13, 'TAU' );
	assertArrayClose( Array.from( T ), tc.T, 1e-13, 'T' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-13, 'Y' );
});

test( 'dlahr2: N=1 (quick return)', function () {
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 5.0 ]);
	var tau = new Float64Array([ 99.0 ]);
	var T = new Float64Array([ 99.0 ]);
	var Y = new Float64Array([ 99.0 ]);
	dlahr2( 1, 1, 1, A, 1, 1, 0, tau, 1, 0, T, 1, 0, 1, Y, 1, 0, 1 );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	// tau should be unchanged since quick return
	assert.equal( tau[0], 99.0, 'tau unchanged' );
});

test( 'dlahr2: 7x7, K=2, NB=2 (non-unit K offset)', function () {
	var tc = findCase( '7x7_k2_nb2' );
	var N = 7; var K = 2; var NB = 2;
	var LDA = N; var LDT = NB; var LDY = N;
	var A = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		A[ i * N + i ] = ( i + 1 ) * 2;
	}
	A[1]  = 1.0; A[2]  = 2.0; A[3]  = 0.5; A[4]  = 1.0;
	A[8]  = 1.5; A[9]  = 1.0; A[10] = 0.5; A[11] = 2.0;
	A[15] = 2.0; A[16] = 0.5; A[17] = 1.0; A[18] = 0.5;
	A[22] = 1.0; A[23] = 0.5; A[24] = 2.0; A[25] = 1.0;
	A[29] = 0.5; A[30] = 1.0; A[31] = 0.5; A[32] = 2.0;
	A[36] = 2.0; A[37] = 0.5; A[38] = 1.0; A[39] = 0.5;
	A[43] = 1.0; A[44] = 2.0; A[45] = 0.5; A[46] = 1.0;
	A[5]  = 0.5; A[6]  = 1.0;
	A[12] = 2.0; A[13] = 0.5;
	A[19] = 1.0; A[20] = 2.0;
	A[26] = 0.5; A[27] = 1.0;
	A[33] = 2.0; A[34] = 0.5;
	A[40] = 1.0; A[41] = 2.0;
	A[47] = 0.5; A[48] = 1.0;
	var tau = new Float64Array( NB );
	var T = new Float64Array( LDT * NB );
	var Y = new Float64Array( LDY * NB );
	dlahr2( N, K, NB, A, 1, LDA, 0, tau, 1, 0, T, 1, 0, LDT, Y, 1, 0, LDY );
	assertArrayClose( Array.from( A ), tc.A, 1e-13, 'A' );
	assertArrayClose( Array.from( tau ), tc.TAU, 1e-13, 'TAU' );
	assertArrayClose( Array.from( T ), tc.T, 1e-13, 'T' );
	assertArrayClose( Array.from( Y ), tc.Y, 1e-13, 'Y' );
});
