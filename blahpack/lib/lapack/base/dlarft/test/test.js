'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlarft = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarft.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i, relErr;
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		if ( relErr > tol ) {
			throw new Error( msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		}
	}
}

// Fortran print_matrix extracts MxN submatrix from LDA-strided storage.
// In JS we use exact-size storage (LDT=K, strideT1=1, strideT2=K).

test( 'dlarft: fwd col 5x3', function t() {
	var tc = findCase( 'fwd_col_5x3' );
	var V = new Float64Array( 6 * 3 );
	V[ 0 + 0 * 6 ] = 1;
	V[ 1 + 0 * 6 ] = 0.5; V[ 1 + 1 * 6 ] = 1;
	V[ 2 + 0 * 6 ] = 0.25; V[ 2 + 1 * 6 ] = 0.5; V[ 2 + 2 * 6 ] = 1;
	V[ 3 + 0 * 6 ] = 0.125; V[ 3 + 1 * 6 ] = 0.25; V[ 3 + 2 * 6 ] = 0.5;
	V[ 4 + 0 * 6 ] = 0.0625; V[ 4 + 1 * 6 ] = 0.125; V[ 4 + 2 * 6 ] = 0.25;
	var TAU = new Float64Array( [ 1.2, 1.5, 1.1 ] );
	var T = new Float64Array( 3 * 3 );
	dlarft( 'F', 'C', 5, 3, V, 1, 6, 0, TAU, 1, 0, T, 1, 3, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: fwd col 3x2', function t() {
	var tc = findCase( 'fwd_col_3x2' );
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 1;
	V[ 1 + 0 * 6 ] = 2; V[ 1 + 1 * 6 ] = 1;
	V[ 2 + 0 * 6 ] = 3; V[ 2 + 1 * 6 ] = 4;
	var TAU = new Float64Array( [ 0.8, 1.2 ] );
	// Use LDT=2 (K=2), so strideT1=1, strideT2=2
	var T = new Float64Array( 2 * 2 );
	dlarft( 'F', 'C', 3, 2, V, 1, 6, 0, TAU, 1, 0, T, 1, 2, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: bwd col 5x2', function t() {
	var tc = findCase( 'bwd_col_5x2' );
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 0.25;
	V[ 1 + 0 * 6 ] = 0.25; V[ 1 + 1 * 6 ] = 0.125;
	V[ 2 + 0 * 6 ] = 0.125; V[ 2 + 1 * 6 ] = 0.0625;
	V[ 3 + 0 * 6 ] = 1.0;
	V[ 4 + 1 * 6 ] = 1.0;
	var TAU = new Float64Array( [ 1.5, 0.9 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'B', 'C', 5, 2, V, 1, 6, 0, TAU, 1, 0, T, 1, 2, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: fwd col tau zero', function t() {
	var tc = findCase( 'fwd_col_tau_zero' );
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 1;
	V[ 1 + 0 * 6 ] = 0.5; V[ 1 + 1 * 6 ] = 1;
	V[ 2 + 0 * 6 ] = 0.25; V[ 2 + 1 * 6 ] = 0.5;
	var TAU = new Float64Array( [ 1.2, 0.0 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'F', 'C', 3, 2, V, 1, 6, 0, TAU, 1, 0, T, 1, 2, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});
