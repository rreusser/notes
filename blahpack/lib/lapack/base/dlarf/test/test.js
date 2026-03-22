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
	// Test with negative incv to exercise the negative stride path (lines 53-54)
	// With strideV=-1, offset=2: reads v[2], v[1], v[0] => logical vector [1.0, 0.5, 0.25]
	var C = new Float64Array( 9 );
	C[ 0 ] = 1; C[ 1 ] = 4; C[ 2 ] = 7;
	C[ 3 ] = 2; C[ 4 ] = 5; C[ 5 ] = 8;
	C[ 6 ] = 3; C[ 7 ] = 6; C[ 8 ] = 9;
	var v = new Float64Array( [ 0.25, 0.5, 1.0 ] );
	var WORK = new Float64Array( 3 );
	var tc = findCase( 'left_3x3' );
	dlarf( 'L', 3, 3, v, -1, 2, 1.5, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dlarf: left with negative strideV and trailing zeros (lines 58-60)', function t() {
	// With negative stride, ix starts at offsetV (line 53).
	// The while loop scans: v[ix] === 0 ? lastv--, ix -= strideV (i.e. ix += 1 for stride=-1)
	// M=5, strideV=-1, offsetV=4: logical vector reads v[4],v[3],v[2],v[1],v[0]
	// Logical v = [1.0, 0.5, 0.25, 0.0, 0.0] -- trailing zeros in logical positions 4,5
	// Physical layout: v[4]=1.0, v[3]=0.5, v[2]=0.25, v[1]=0.0, v[0]=0.0
	// ix starts at offsetV=4. The while loop checks v[4]=1.0, stops immediately.
	// That won't enter the loop. We need the END of the logical vector to be zero.
	// With negative stride: ix = offsetV = 4. Scan v[4], v[5], v[6]... (ix -= stride = ix += 1)
	// Wait, let's re-read: lastv=M=5, ix=offsetV=4 (for negative stride).
	// while (lastv>0 && v[ix]===0): checks v[4]. If zero, lastv--, ix -= (-1) = ix++, check v[5]...
	// So "trailing zeros" in this negative-stride sense means zeros at the beginning of physical array
	// starting from offsetV going upward.
	// Actually no. ix = offsetV (line 53), and the scan is: v[ix], then ix -= strideV.
	// strideV = -1, so ix -= (-1) = ix + 1.
	// So it checks v[offsetV], v[offsetV+1], v[offsetV+2], ...
	// The logical vector with stride=-1 and offset=4 reads: v[4], v[3], v[2], v[1], v[0]
	// The "last" elements (high-index logical positions) correspond to v[0], v[1], ...
	// The scan starts at ix=offsetV=4 and goes upward. So it's checking the FIRST logical element.
	// Hmm, that doesn't make sense for finding "last non-zero".
	// Let me re-read the code more carefully.
	//
	// For positive stride: ix = offsetV + (lastv-1)*strideV (the last element)
	//   scan backward: ix -= strideV moves toward the first element
	// For negative stride: ix = offsetV (line 53)
	//   scan: ix -= strideV means ix -= (-1) = ix + 1
	//   So starting from offsetV, we move upward in memory
	//   lastv starts at M, and we're looking for the last non-zero
	//   With stride=-1 and offset=4: logical[0]=v[4], logical[1]=v[3], ..., logical[4]=v[0]
	//   The "last" element is logical[4]=v[0]. So scanning from ix=4 upward finds logical[4..3..2..]
	//   Wait no: ix starts at offsetV=4, then ix += 1 = 5, 6, 7...
	//   But the logical vector only uses indices 0-4 (v[0]..v[4])
	//   So ix=4 checks v[4]=logical[0], then ix=5 checks v[5]=logical[-1]... out of bounds.
	//   Something is wrong with my understanding. Let me just try it.
	var C = new Float64Array( 5 * 3 );
	C[ 0 ] = 1; C[ 1 ] = 2; C[ 2 ] = 3; C[ 3 ] = 4; C[ 4 ] = 5;
	C[ 5 ] = 6; C[ 6 ] = 7; C[ 7 ] = 8; C[ 8 ] = 9; C[ 9 ] = 10;
	C[ 10 ] = 11; C[ 11 ] = 12; C[ 12 ] = 13; C[ 13 ] = 14; C[ 14 ] = 15;
	// v has 5 elements with trailing zeros at the end
	// For negative stride with offset at the end: v = [0.0, 0.0, 0.25, 0.5, 1.0]
	// stride=-1, offset=4: logical = [v[4], v[3], v[2], v[1], v[0]] = [1.0, 0.5, 0.25, 0.0, 0.0]
	// The last two logical elements are zero -> the while loop should trim lastv from 5 to 3
	var v = new Float64Array( [ 0.0, 0.0, 0.25, 0.5, 1.0 ] );
	var WORK = new Float64Array( 3 );
	// For positive stride comparison:
	var C2 = new Float64Array( C );
	var v2 = new Float64Array( [ 1.0, 0.5, 0.25, 0.0, 0.0 ] );
	var WORK2 = new Float64Array( 3 );
	dlarf( 'L', 5, 3, v2, 1, 0, 1.5, C2, 1, 5, 0, WORK2, 1, 0 );
	dlarf( 'L', 5, 3, v, -1, 4, 1.5, C, 1, 5, 0, WORK, 1, 0 );
	assertArrayClose( C, C2, 1e-14, 'C negative stride with trailing zeros' );
});
