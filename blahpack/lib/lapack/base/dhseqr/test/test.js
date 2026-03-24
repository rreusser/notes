

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dhseqr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dhseqr.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Builds an N-by-N column-major matrix from Fortran-style assignments.
* Returns a Float64Array of length N*N with stride1=1, stride2=N.
*
* @param {NonNegativeInteger} N - matrix dimension
* @param {Array} entries - array of [row, col, value] (0-based)
* @returns {Float64Array} column-major matrix
*/
function makeMatrix( N, entries ) {
	var A = new Float64Array( N * N );
	var k;
	for ( k = 0; k < entries.length; k++ ) {
		A[ entries[ k ][ 0 ] + ( entries[ k ][ 1 ] * N ) ] = entries[ k ][ 2 ];
	}
	return A;
}

/**
* Extracts a 1D array from a column-major matrix.
*
* @param {Float64Array} A - column-major matrix
* @param {integer} N - matrix dimension
* @returns {Array} extracted column-major values as plain array
*/
function matrixToArray( A, N ) {
	var result = [];
	var i;
	for ( i = 0; i < N * N; i++ ) {
		result.push( A[ i ] );
	}
	return result;
}


// TESTS //

test( 'dhseqr: eigenvalues_only_6x6', function t() {
	var tc = findCase( 'eigenvalues_only_6x6' );
	var N = 6;
	var H = makeMatrix( N, [
		[ 0, 0, 4.0 ], [ 0, 1, 3.0 ], [ 0, 2, 2.0 ], [ 0, 3, 1.0 ], [ 0, 4, 0.5 ], [ 0, 5, 0.1 ],
		[ 1, 0, 1.0 ], [ 1, 1, 4.0 ], [ 1, 2, 3.0 ], [ 1, 3, 2.0 ], [ 1, 4, 1.0 ], [ 1, 5, 0.5 ],
		[ 2, 1, 1.0 ], [ 2, 2, 4.0 ], [ 2, 3, 3.0 ], [ 2, 4, 2.0 ], [ 2, 5, 1.0 ],
		[ 3, 2, 1.0 ], [ 3, 3, 4.0 ], [ 3, 4, 3.0 ], [ 3, 5, 2.0 ],
		[ 4, 3, 1.0 ], [ 4, 4, 4.0 ], [ 4, 5, 3.0 ],
		[ 5, 4, 1.0 ], [ 5, 5, 4.0 ]
	]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var Z = new Float64Array( N * N );

	var info = dhseqr( 'eigenvalues', 'none', N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, Z, 1, N, 0 );

	assert.equal( info, tc.info, 'info' );

	// Sort eigenvalues for comparison since order may differ
	var actualWR = Array.from( WR ).sort( function( a, b ) { return a - b; } );
	var expectedWR = tc.wr.slice().sort( function( a, b ) { return a - b; } );
	assertArrayClose( actualWR, expectedWR, 1e-12, 'wr' );

	var actualWI = Array.from( WI ).sort( function( a, b ) { return a - b; } );
	var expectedWI = tc.wi.slice().sort( function( a, b ) { return a - b; } );
	assertArrayClose( actualWI, expectedWI, 1e-12, 'wi' );
});

test( 'dhseqr: schur_with_z_init_4x4', function t() {
	var tc = findCase( 'schur_with_z_init_4x4' );
	var N = 4;
	var H = makeMatrix( N, [
		[ 0, 0, 4.0 ], [ 0, 1, 3.0 ], [ 0, 2, 2.0 ], [ 0, 3, 1.0 ],
		[ 1, 0, 1.0 ], [ 1, 1, 4.0 ], [ 1, 2, 3.0 ], [ 1, 3, 2.0 ],
		[ 2, 1, 1.0 ], [ 2, 2, 4.0 ], [ 2, 3, 3.0 ],
		[ 3, 2, 1.0 ], [ 3, 3, 4.0 ]
	]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var Z = new Float64Array( N * N );

	var info = dhseqr( 'schur', 'initialize', N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, Z, 1, N, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-12, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-12, 'wi' );
	assertArrayClose( matrixToArray( H, N ), tc.h, 1e-12, 'h' );
	assertArrayClose( matrixToArray( Z, N ), tc.z, 1e-12, 'z' );
});

test( 'dhseqr: schur_with_z_update_4x4', function t() {
	var tc = findCase( 'schur_with_z_update_4x4' );
	var N = 4;
	var H = makeMatrix( N, [
		[ 0, 0, 2.0 ], [ 0, 1, 1.0 ], [ 0, 2, 0.5 ], [ 0, 3, 0.1 ],
		[ 1, 0, 3.0 ], [ 1, 1, 1.0 ], [ 1, 2, 2.0 ], [ 1, 3, 0.5 ],
		[ 2, 1, 1.0 ], [ 2, 2, 3.0 ], [ 2, 3, 1.0 ],
		[ 3, 2, 0.5 ], [ 3, 3, 4.0 ]
	]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	// Initialize Z to identity (COMPZ='V' expects Z on input)
	var Z = makeMatrix( N, [
		[ 0, 0, 1.0 ], [ 1, 1, 1.0 ], [ 2, 2, 1.0 ], [ 3, 3, 1.0 ]
	]);

	var info = dhseqr( 'schur', 'update', N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, Z, 1, N, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-12, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-12, 'wi' );
	assertArrayClose( matrixToArray( H, N ), tc.h, 1e-12, 'h' );
	assertArrayClose( matrixToArray( Z, N ), tc.z, 1e-12, 'z' );
});

test( 'dhseqr: schur_no_z_4x4', function t() {
	var tc = findCase( 'schur_no_z_4x4' );
	var N = 4;
	var H = makeMatrix( N, [
		[ 0, 0, 4.0 ], [ 0, 1, 3.0 ], [ 0, 2, 2.0 ], [ 0, 3, 1.0 ],
		[ 1, 0, 1.0 ], [ 1, 1, 4.0 ], [ 1, 2, 3.0 ], [ 1, 3, 2.0 ],
		[ 2, 1, 1.0 ], [ 2, 2, 4.0 ], [ 2, 3, 3.0 ],
		[ 3, 2, 1.0 ], [ 3, 3, 4.0 ]
	]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var Z = new Float64Array( N * N );

	var info = dhseqr( 'schur', 'none', N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, Z, 1, N, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-12, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-12, 'wi' );
	assertArrayClose( matrixToArray( H, N ), tc.h, 1e-12, 'h' );
});

test( 'dhseqr: n0', function t() {
	var WR = new Float64Array( 1 );
	var WI = new Float64Array( 1 );
	var H = new Float64Array( 1 );
	var Z = new Float64Array( 1 );

	var info = dhseqr( 'schur', 'initialize', 0, 1, 0, H, 1, 1, 0, WR, 1, 0, WI, 1, 0, Z, 1, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dhseqr: n1', function t() {
	var tc = findCase( 'n1' );
	var N = 1;
	var H = new Float64Array( [ 7.0 ] );
	var WR = new Float64Array( 1 );
	var WI = new Float64Array( 1 );
	var Z = new Float64Array( 1 );

	var info = dhseqr( 'schur', 'initialize', N, 1, N, H, 1, 1, 0, WR, 1, 0, WI, 1, 0, Z, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertClose( WR[ 0 ], tc.wr1, 1e-14, 'wr1' );
	assertClose( WI[ 0 ], tc.wi1, 1e-14, 'wi1' );
	assertClose( H[ 0 ], tc.h11, 1e-14, 'h11' );
	assertClose( Z[ 0 ], tc.z11, 1e-14, 'z11' );
});

test( 'dhseqr: n2_complex', function t() {
	var tc = findCase( 'n2_complex' );
	var N = 2;
	var H = makeMatrix( N, [
		[ 0, 0, 0.0 ], [ 0, 1, -2.0 ],
		[ 1, 0, 1.0 ], [ 1, 1, 0.0 ]
	]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var Z = new Float64Array( N * N );

	var info = dhseqr( 'schur', 'initialize', N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, Z, 1, N, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-12, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-12, 'wi' );
	assertArrayClose( matrixToArray( H, N ), tc.h, 1e-12, 'h' );
	assertArrayClose( matrixToArray( Z, N ), tc.z, 1e-12, 'z' );
});

test( 'dhseqr: ilo_eq_ihi', function t() {
	var tc = findCase( 'ilo_eq_ihi' );
	var N = 4;
	var H = makeMatrix( N, [
		[ 0, 0, 5.0 ], [ 0, 1, 3.0 ], [ 0, 2, 2.0 ], [ 0, 3, 1.0 ],
		[ 1, 1, 4.0 ], [ 1, 2, 3.0 ], [ 1, 3, 2.0 ],
		[ 2, 2, 3.0 ], [ 2, 3, 1.0 ],
		[ 3, 3, 7.0 ]
	]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var Z = new Float64Array( N * N );

	var info = dhseqr( 'schur', 'none', N, 2, 2, H, 1, N, 0, WR, 1, 0, WI, 1, 0, Z, 1, N, 0 );

	assert.equal( info, tc.info, 'info' );
	assertClose( WR[ 1 ], tc.wr2, 1e-14, 'wr2' );
	assertClose( WI[ 1 ], tc.wi2, 1e-14, 'wi2' );
});

test( 'dhseqr: complex_pairs_6x6', function t() {
	var tc = findCase( 'complex_pairs_6x6' );
	var N = 6;
	var H = makeMatrix( N, [
		[ 0, 0, 1.0 ], [ 0, 1, -2.0 ], [ 0, 2, 1.0 ], [ 0, 3, 0.5 ], [ 0, 4, 0.1 ], [ 0, 5, 0.2 ],
		[ 1, 0, 2.0 ], [ 1, 1, 1.0 ], [ 1, 2, -1.0 ], [ 1, 3, 0.3 ], [ 1, 4, 0.4 ], [ 1, 5, 0.1 ],
		[ 2, 1, 1.5 ], [ 2, 2, 2.0 ], [ 2, 3, -1.0 ], [ 2, 4, 0.5 ], [ 2, 5, 0.3 ],
		[ 3, 2, 1.0 ], [ 3, 3, 3.0 ], [ 3, 4, -2.0 ], [ 3, 5, 0.4 ],
		[ 4, 3, 2.0 ], [ 4, 4, 1.0 ], [ 4, 5, -1.0 ],
		[ 5, 4, 1.0 ], [ 5, 5, 2.0 ]
	]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var Z = new Float64Array( N * N );

	var info = dhseqr( 'schur', 'initialize', N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, Z, 1, N, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-12, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-12, 'wi' );
	assertArrayClose( matrixToArray( H, N ), tc.h, 1e-12, 'h' );
	assertArrayClose( matrixToArray( Z, N ), tc.z, 1e-12, 'z' );
});

test( 'dhseqr: partial_range_6x6', function t() {
	var tc = findCase( 'partial_range_6x6' );
	var N = 6;
	var H = makeMatrix( N, [
		[ 0, 0, 10.0 ], [ 0, 1, 1.0 ], [ 0, 2, 2.0 ],
		[ 0, 3, 3.0 ], [ 0, 4, 4.0 ], [ 0, 5, 5.0 ],
		[ 1, 1, 4.0 ], [ 1, 2, 3.0 ], [ 1, 3, 1.0 ], [ 1, 4, 0.5 ], [ 1, 5, 0.1 ],
		[ 2, 1, 1.0 ], [ 2, 2, 3.0 ], [ 2, 3, 2.0 ], [ 2, 4, 1.0 ], [ 2, 5, 0.2 ],
		[ 3, 2, 0.5 ], [ 3, 3, 2.0 ], [ 3, 4, 1.5 ], [ 3, 5, 0.3 ],
		[ 4, 3, 0.25 ], [ 4, 4, 1.0 ], [ 4, 5, 0.4 ],
		[ 5, 5, 20.0 ]
	]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var Z = new Float64Array( N * N );

	var info = dhseqr( 'schur', 'initialize', N, 2, 5, H, 1, N, 0, WR, 1, 0, WI, 1, 0, Z, 1, N, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-12, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-12, 'wi' );
	assertArrayClose( matrixToArray( H, N ), tc.h, 1e-12, 'h' );
	assertArrayClose( matrixToArray( Z, N ), tc.z, 1e-12, 'z' );
});
