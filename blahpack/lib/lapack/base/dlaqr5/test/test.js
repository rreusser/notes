'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlaqr5 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaqr5.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] + ' (relErr=' + relErr + ')' );
	}
}

/**
* Extracts an NxN submatrix from a flat column-major array (stride=LDA) into a flat column-major
* array with stride=N. This mirrors what Fortran's print_matrix outputs.
*
* @private
*/
function extractMatrix( A, N, LDA ) {
	var out = new Float64Array( N * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			out[ j * N + i ] = A[ j * LDA + i ];
		}
	}
	return out;
}

/**
* Creates a column-major NxN matrix (leading dimension = N) from Fortran-style
* row assignments. Returns Float64Array of size N*N.
*
* @private
*/
function colMajor( N, entries ) {
	var A = new Float64Array( N * N );
	var key;
	var row;
	var col;
	for ( key in entries ) {
		row = parseInt( key.split( ',' )[ 0 ], 10 ) - 1; // 0-based
		col = parseInt( key.split( ',' )[ 1 ], 10 ) - 1;
		A[ col * N + row ] = entries[ key ];
	}
	return A;
}

/**
* Creates identity matrix in column-major flat format.
*
* @private
*/
function eye( N ) {
	var A = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		A[ i * N + i ] = 1.0;
	}
	return A;
}


// TESTS //

test( 'dlaqr5: 6x6_2shifts_wantt_wantz', function t() {
	var tc = findCase( '6x6_2shifts_wantt_wantz' );
	var N = 6;
	var nshfts = 2;
	var H = colMajor( N, {
		'1,1': 4.0, '1,2': 3.0, '1,3': 2.0, '1,4': 1.0, '1,5': 0.5, '1,6': 0.25,
		'2,1': 1.0, '2,2': 3.0, '2,3': 2.5, '2,4': 1.5, '2,5': 0.8, '2,6': 0.4,
		'3,2': 1.5, '3,3': 2.0, '3,4': 1.0, '3,5': 0.7, '3,6': 0.3,
		'4,3': 0.8, '4,4': 1.5, '4,5': 0.6, '4,6': 0.2,
		'5,4': 0.5, '5,5': 1.0, '5,6': 0.5,
		'6,5': 0.3, '6,6': 0.5
	});
	var Z = eye( N );
	var SR = new Float64Array([ 2.0, 2.0 ]);
	var SI = new Float64Array([ 0.5, -0.5 ]);
	var V = new Float64Array( 3 * nshfts );
	var U = new Float64Array( N * N );
	var WV = new Float64Array( N * N );
	var WH = new Float64Array( N * N );

	// ktop=0, kbot=5 (0-based), iloz=0, ihiz=5
	dlaqr5( true, true, 0, N, 0, 5, nshfts,
		SR, 1, 0, SI, 1, 0,
		H, 1, N, 0,
		0, 5,
		Z, 1, N, 0,
		V, 1, 3, 0,
		U, 1, N, 0,
		N, WV, 1, N, 0,
		N, WH, 1, N, 0 );

	assertArrayClose( Array.from( extractMatrix( H, N, N ) ), tc.H, 1e-12, 'H' );
	assertArrayClose( Array.from( extractMatrix( Z, N, N ) ), tc.Z, 1e-12, 'Z' );
	assertArrayClose( Array.from( SR ), tc.SR, 1e-14, 'SR' );
	assertArrayClose( Array.from( SI ), tc.SI, 1e-14, 'SI' );
});

test( 'dlaqr5: 8x8_4shifts_kacc22_1', function t() {
	var tc = findCase( '8x8_4shifts_kacc22_1' );
	var N = 8;
	var nshfts = 4;
	var H = colMajor( N, {
		'1,1': 10.0, '1,2': 2.0, '1,3': 1.0, '1,4': 0.5, '1,5': 0.3, '1,6': 0.2, '1,7': 0.1, '1,8': 0.05,
		'2,1': 3.0, '2,2': 9.0, '2,3': 2.0, '2,4': 1.0, '2,5': 0.4, '2,6': 0.3, '2,7': 0.2, '2,8': 0.1,
		'3,2': 2.5, '3,3': 8.0, '3,4': 1.5, '3,5': 0.5, '3,6': 0.4, '3,7': 0.3, '3,8': 0.15,
		'4,3': 2.0, '4,4': 7.0, '4,5': 1.0, '4,6': 0.5, '4,7': 0.4, '4,8': 0.2,
		'5,4': 1.5, '5,5': 6.0, '5,6': 1.0, '5,7': 0.5, '5,8': 0.3,
		'6,5': 1.0, '6,6': 5.0, '6,7': 0.8, '6,8': 0.4,
		'7,6': 0.8, '7,7': 4.0, '7,8': 0.6,
		'8,7': 0.5, '8,8': 3.0
	});
	var Z = eye( N );
	var SR = new Float64Array([ 5.0, 5.0, 3.0, 3.0 ]);
	var SI = new Float64Array([ 1.0, -1.0, 0.5, -0.5 ]);
	var V = new Float64Array( 3 * nshfts );
	var U = new Float64Array( N * N );
	var WV = new Float64Array( N * N );
	var WH = new Float64Array( N * N );

	dlaqr5( true, true, 1, N, 0, 7, nshfts,
		SR, 1, 0, SI, 1, 0,
		H, 1, N, 0,
		0, 7,
		Z, 1, N, 0,
		V, 1, 3, 0,
		U, 1, N, 0,
		N, WV, 1, N, 0,
		N, WH, 1, N, 0 );

	assertArrayClose( Array.from( extractMatrix( H, N, N ) ), tc.H, 1e-12, 'H' );
	assertArrayClose( Array.from( extractMatrix( Z, N, N ) ), tc.Z, 1e-12, 'Z' );
	assertArrayClose( Array.from( SR ), tc.SR, 1e-14, 'SR' );
	assertArrayClose( Array.from( SI ), tc.SI, 1e-14, 'SI' );
});

test( 'dlaqr5: 6x6_no_wantt_no_wantz', function t() {
	var tc = findCase( '6x6_no_wantt_no_wantz' );
	var N = 6;
	var nshfts = 2;
	var H = colMajor( N, {
		'1,1': 4.0, '1,2': 3.0, '1,3': 2.0, '1,4': 1.0, '1,5': 0.5, '1,6': 0.25,
		'2,1': 1.0, '2,2': 3.0, '2,3': 2.5, '2,4': 1.5, '2,5': 0.8, '2,6': 0.4,
		'3,2': 1.5, '3,3': 2.0, '3,4': 1.0, '3,5': 0.7, '3,6': 0.3,
		'4,3': 0.8, '4,4': 1.5, '4,5': 0.6, '4,6': 0.2,
		'5,4': 0.5, '5,5': 1.0, '5,6': 0.5,
		'6,5': 0.3, '6,6': 0.5
	});
	var Z = new Float64Array( N * N );
	var SR = new Float64Array([ 2.0, 2.0 ]);
	var SI = new Float64Array([ 0.5, -0.5 ]);
	var V = new Float64Array( 3 * nshfts );
	var U = new Float64Array( N * N );
	var WV = new Float64Array( N * N );
	var WH = new Float64Array( N * N );

	dlaqr5( false, false, 0, N, 0, 5, nshfts,
		SR, 1, 0, SI, 1, 0,
		H, 1, N, 0,
		0, 5,
		Z, 1, N, 0,
		V, 1, 3, 0,
		U, 1, N, 0,
		N, WV, 1, N, 0,
		N, WH, 1, N, 0 );

	assertArrayClose( Array.from( extractMatrix( H, N, N ) ), tc.H, 1e-12, 'H' );
});

test( 'dlaqr5: nshfts_0_noop', function t() {
	var tc = findCase( 'nshfts_0_noop' );
	var N = 4;
	var H = colMajor( N, {
		'1,1': 4.0, '1,2': 1.0, '1,3': 0.5, '1,4': 0.25,
		'2,1': 2.0, '2,2': 3.0, '2,3': 1.0, '2,4': 0.5,
		'3,2': 1.5, '3,3': 2.0, '3,4': 0.8,
		'4,3': 1.0, '4,4': 1.0
	});
	var Horig = new Float64Array( H );
	var Z = new Float64Array( N * N );
	var SR = new Float64Array( 4 );
	var SI = new Float64Array( 4 );
	var V = new Float64Array( 12 );
	var U = new Float64Array( N * N );
	var WV = new Float64Array( N * N );
	var WH = new Float64Array( N * N );

	dlaqr5( true, true, 0, N, 0, 3, 0,
		SR, 1, 0, SI, 1, 0,
		H, 1, N, 0,
		0, 3,
		Z, 1, N, 0,
		V, 1, 3, 0,
		U, 1, N, 0,
		N, WV, 1, N, 0,
		N, WH, 1, N, 0 );

	// H should be unchanged
	assertArrayClose( Array.from( extractMatrix( H, N, N ) ), tc.H, 1e-14, 'H' );
});

test( 'dlaqr5: ktop_eq_kbot_noop', function t() {
	var tc = findCase( 'ktop_eq_kbot_noop' );
	var N = 4;
	var H = colMajor( N, {
		'1,1': 4.0, '1,2': 1.0, '1,3': 0.5, '1,4': 0.25,
		'2,1': 2.0, '2,2': 3.0, '2,3': 1.0, '2,4': 0.5,
		'3,2': 1.5, '3,3': 2.0, '3,4': 0.8,
		'4,3': 1.0, '4,4': 1.0
	});
	var Z = new Float64Array( N * N );
	var SR = new Float64Array([ 2.0, 1.0 ]);
	var SI = new Float64Array([ 0.0, 0.0 ]);
	var V = new Float64Array( 6 );
	var U = new Float64Array( N * N );
	var WV = new Float64Array( N * N );
	var WH = new Float64Array( N * N );

	// ktop=2 (0-based), kbot=2 (0-based) => KTOP=KBOT=3, so ktop >= kbot => no-op
	dlaqr5( true, true, 0, N, 2, 2, 2,
		SR, 1, 0, SI, 1, 0,
		H, 1, N, 0,
		0, 3,
		Z, 1, N, 0,
		V, 1, 3, 0,
		U, 1, N, 0,
		N, WV, 1, N, 0,
		N, WH, 1, N, 0 );

	assertArrayClose( Array.from( extractMatrix( H, N, N ) ), tc.H, 1e-14, 'H' );
});

test( 'dlaqr5: 6x6_4shifts_kacc22_2', function t() {
	var tc = findCase( '6x6_4shifts_kacc22_2' );
	var N = 6;
	var nshfts = 4;
	var H = colMajor( N, {
		'1,1': 6.0, '1,2': 2.0, '1,3': 1.5, '1,4': 1.0, '1,5': 0.5, '1,6': 0.3,
		'2,1': 2.0, '2,2': 5.0, '2,3': 2.0, '2,4': 1.5, '2,5': 0.8, '2,6': 0.4,
		'3,2': 1.8, '3,3': 4.0, '3,4': 1.0, '3,5': 0.6, '3,6': 0.35,
		'4,3': 1.2, '4,4': 3.0, '4,5': 0.9, '4,6': 0.5,
		'5,4': 0.7, '5,5': 2.0, '5,6': 0.6,
		'6,5': 0.4, '6,6': 1.0
	});
	var Z = eye( N );
	var SR = new Float64Array([ 4.0, 4.0, 2.0, 1.0 ]);
	var SI = new Float64Array([ 1.0, -1.0, 0.0, 0.0 ]);
	var V = new Float64Array( 3 * nshfts );
	var U = new Float64Array( N * N );
	var WV = new Float64Array( N * N );
	var WH = new Float64Array( N * N );

	dlaqr5( true, true, 2, N, 0, 5, nshfts,
		SR, 1, 0, SI, 1, 0,
		H, 1, N, 0,
		0, 5,
		Z, 1, N, 0,
		V, 1, 3, 0,
		U, 1, N, 0,
		N, WV, 1, N, 0,
		N, WH, 1, N, 0 );

	assertArrayClose( Array.from( extractMatrix( H, N, N ) ), tc.H, 1e-12, 'H' );
	assertArrayClose( Array.from( extractMatrix( Z, N, N ) ), tc.Z, 1e-12, 'Z' );
	assertArrayClose( Array.from( SR ), tc.SR, 1e-14, 'SR' );
	assertArrayClose( Array.from( SI ), tc.SI, 1e-14, 'SI' );
});

test( 'dlaqr5: 6x6_partial_sweep', function t() {
	var tc = findCase( '6x6_partial_sweep' );
	var N = 6;
	var nshfts = 2;
	var H = colMajor( N, {
		'1,1': 5.0, '1,2': 1.0, '1,3': 0.5, '1,4': 0.3, '1,5': 0.2, '1,6': 0.1,
		'2,1': 0.0, '2,2': 4.0, '2,3': 2.0, '2,4': 1.0, '2,5': 0.5, '2,6': 0.3,
		'3,2': 1.5, '3,3': 3.0, '3,4': 1.5, '3,5': 0.7, '3,6': 0.4,
		'4,3': 1.0, '4,4': 2.0, '4,5': 1.0, '4,6': 0.5,
		'5,4': 0.8, '5,5': 1.5, '5,6': 0.6,
		'6,5': 0.0, '6,6': 1.0
	});
	var Z = eye( N );
	var SR = new Float64Array([ 3.0, 2.0 ]);
	var SI = new Float64Array([ 0.0, 0.0 ]);
	var V = new Float64Array( 3 * nshfts );
	var U = new Float64Array( N * N );
	var WV = new Float64Array( N * N );
	var WH = new Float64Array( N * N );

	// ktop=1 (0-based, Fortran KTOP=2), kbot=4 (0-based, Fortran KBOT=5)
	dlaqr5( true, true, 0, N, 1, 4, nshfts,
		SR, 1, 0, SI, 1, 0,
		H, 1, N, 0,
		0, 5,
		Z, 1, N, 0,
		V, 1, 3, 0,
		U, 1, N, 0,
		N, WV, 1, N, 0,
		N, WH, 1, N, 0 );

	assertArrayClose( Array.from( extractMatrix( H, N, N ) ), tc.H, 1e-12, 'H' );
	assertArrayClose( Array.from( extractMatrix( Z, N, N ) ), tc.Z, 1e-12, 'Z' );
});
