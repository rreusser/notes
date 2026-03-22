'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlarf = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// C is 3x3 or 2x3 col-major, LDA=4 in Fortran -> we use LDA=3 or LDA=2 for exact packing

test( 'dlarf: left 3x3', function t() {
	var tc = findCase( 'left_3x3' );
	// LDA=4, but C is stored column major with LDA=4 rows.
	// We need to match Fortran layout: C stored in col-major with LDA=4
	// strideC1=1, strideC2=4 but only 3 rows used
	// Simpler: pack tightly with strideC1=1, strideC2=3
	// But Fortran uses LDA=4, so fixture has 3x3 values (9 values)
	var C = new Float64Array( 9 );
	C[ 0 ] = 1; C[ 1 ] = 4; C[ 2 ] = 7;
	C[ 3 ] = 2; C[ 4 ] = 5; C[ 5 ] = 8;
	C[ 6 ] = 3; C[ 7 ] = 6; C[ 8 ] = 9;
	var v = new Float64Array( [ 1.0, 0.5, 0.25 ] );
	var WORK = new Float64Array( 3 );
	dlarf( 'L', 3, 3, v, 1, 0, 1.5, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dlarf: right 3x3', function t() {
	var tc = findCase( 'right_3x3' );
	var C = new Float64Array( 9 );
	C[ 0 ] = 1; C[ 1 ] = 4; C[ 2 ] = 7;
	C[ 3 ] = 2; C[ 4 ] = 5; C[ 5 ] = 8;
	C[ 6 ] = 3; C[ 7 ] = 6; C[ 8 ] = 9;
	var v = new Float64Array( [ 1.0, 0.5, 0.25 ] );
	var WORK = new Float64Array( 3 );
	dlarf( 'R', 3, 3, v, 1, 0, 1.5, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dlarf: tau=0 (identity)', function t() {
	var tc = findCase( 'tau_zero' );
	var C = new Float64Array( 4 );
	C[ 0 ] = 1; C[ 1 ] = 3; C[ 2 ] = 2; C[ 3 ] = 4;
	var v = new Float64Array( [ 1.0, 0.5 ] );
	var WORK = new Float64Array( 2 );
	dlarf( 'L', 2, 2, v, 1, 0, 0.0, C, 1, 2, 0, WORK, 1, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dlarf: left 2x3', function t() {
	var tc = findCase( 'left_2x3' );
	var C = new Float64Array( 6 );
	C[ 0 ] = 1; C[ 1 ] = 4;
	C[ 2 ] = 2; C[ 3 ] = 5;
	C[ 4 ] = 3; C[ 5 ] = 6;
	var v = new Float64Array( [ 1.0, 2.0 ] );
	var WORK = new Float64Array( 3 );
	dlarf( 'L', 2, 3, v, 1, 0, 0.8, C, 1, 2, 0, WORK, 1, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dlarf: left with negative strideV', function t() {
	// Test with negative incv to exercise the negative stride path (lines 53-54, 58-60)
	// v = [1.0, 0.5, 0.25] stored in reverse with stride -1
	// With strideV=-1, offsetV=2: v[2]=0.25, v[1]=0.5, v[0]=1.0
	// The logical vector is [0.25, 0.5, 1.0] (traversed from offsetV with negative stride)
	// Actually: with negative stride, ix = offsetV (line 53), so ix starts at offsetV
	// and the "last non-zero" scan goes: ix -= strideV (i.e. ix += 1)
	var C = new Float64Array( 9 );
	C[ 0 ] = 1; C[ 1 ] = 4; C[ 2 ] = 7;
	C[ 3 ] = 2; C[ 4 ] = 5; C[ 5 ] = 8;
	C[ 6 ] = 3; C[ 7 ] = 6; C[ 8 ] = 9;
	// v stored reversed: logical v is [1.0, 0.5, 0.25]
	// With stride=-1 and offset=2: reads v[2], v[1], v[0]
	var v = new Float64Array( [ 0.25, 0.5, 1.0 ] );
	var WORK = new Float64Array( 3 );
	// Compare with the forward case: same result as left_3x3
	var tc = findCase( 'left_3x3' );
	dlarf( 'L', 3, 3, v, -1, 2, 1.5, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});
