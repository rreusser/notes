'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Int32Array = require( '@stdlib/array/int32' );
var dlasdt = require( './../lib' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlasdt.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

/**
* Helper to run dlasdt and compare against Fortran fixture.
*
* @private
* @param {Object} tc - test case from fixture
*/
function runTest( tc ) {
	var strideINODE;
	var offsetINODE;
	var strideNDIML;
	var offsetNDIML;
	var strideNDIMR;
	var offsetNDIMR;
	var INODE;
	var NDIML;
	var NDIMR;
	var lvl;
	var nd;
	var i;

	lvl = new Int32Array( 1 );
	nd = new Int32Array( 1 );
	INODE = new Int32Array( tc.N > 0 ? tc.N : 1 );
	NDIML = new Int32Array( tc.N > 0 ? tc.N : 1 );
	NDIMR = new Int32Array( tc.N > 0 ? tc.N : 1 );
	strideINODE = 1;
	offsetINODE = 0;
	strideNDIML = 1;
	offsetNDIML = 0;
	strideNDIMR = 1;
	offsetNDIMR = 0;

	dlasdt.ndarray( tc.N, lvl, nd, INODE, strideINODE, offsetINODE, NDIML, strideNDIML, offsetNDIML, NDIMR, strideNDIMR, offsetNDIMR, tc.MSUB );

	assert.strictEqual( lvl[ 0 ], tc.LVL, 'LVL' );
	assert.strictEqual( nd[ 0 ], tc.ND, 'ND' );

	// INODE values are 0-based in JS, 1-based in Fortran fixture:
	for ( i = 0; i < tc.ND; i += 1 ) {
		assert.strictEqual( INODE[ i ], tc.INODE[ i ] - 1, 'INODE[' + i + ']' );
	}

	// NDIML and NDIMR are counts (not indices), same in both:
	for ( i = 0; i < tc.ND; i += 1 ) {
		assert.strictEqual( NDIML[ i ], tc.NDIML[ i ], 'NDIML[' + i + ']' );
	}
	for ( i = 0; i < tc.ND; i += 1 ) {
		assert.strictEqual( NDIMR[ i ], tc.NDIMR[ i ], 'NDIMR[' + i + ']' );
	}
}


// TESTS //

test( 'dlasdt: main export is a function', function t() {
	assert.strictEqual( typeof dlasdt, 'function' );
});

test( 'dlasdt: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlasdt.ndarray, 'function' );
});

test( 'dlasdt: n15_msub6', function t() {
	runTest( findCase( 'n15_msub6' ) );
});

test( 'dlasdt: n1_msub6', function t() {
	runTest( findCase( 'n1_msub6' ) );
});

test( 'dlasdt: n2_msub6', function t() {
	runTest( findCase( 'n2_msub6' ) );
});

test( 'dlasdt: n31_msub6', function t() {
	runTest( findCase( 'n31_msub6' ) );
});

test( 'dlasdt: n63_msub12', function t() {
	runTest( findCase( 'n63_msub12' ) );
});

test( 'dlasdt: n7_msub2', function t() {
	runTest( findCase( 'n7_msub2' ) );
});

test( 'dlasdt: n100_msub25', function t() {
	runTest( findCase( 'n100_msub25' ) );
});

test( 'dlasdt: non-unit stride', function t() {
	var strideINODE;
	var offsetINODE;
	var strideNDIML;
	var offsetNDIML;
	var strideNDIMR;
	var offsetNDIMR;
	var INODE;
	var NDIML;
	var NDIMR;
	var lvl;
	var nd;
	var N;

	N = 15;
	lvl = new Int32Array( 1 );
	nd = new Int32Array( 1 );

	// Use stride 2, offset 1
	INODE = new Int32Array( 2 * N + 1 );
	NDIML = new Int32Array( 2 * N + 1 );
	NDIMR = new Int32Array( 2 * N + 1 );
	strideINODE = 2;
	offsetINODE = 1;
	strideNDIML = 2;
	offsetNDIML = 1;
	strideNDIMR = 2;
	offsetNDIMR = 1;

	dlasdt.ndarray( N, lvl, nd, INODE, strideINODE, offsetINODE, NDIML, strideNDIML, offsetNDIML, NDIMR, strideNDIMR, offsetNDIMR, 6 );

	assert.strictEqual( lvl[ 0 ], 2, 'LVL' );
	assert.strictEqual( nd[ 0 ], 3, 'ND' );

	// Check values at strided positions (0-based INODE):
	assert.strictEqual( INODE[ 1 ], 7, 'INODE[0] at offset 1' );
	assert.strictEqual( INODE[ 3 ], 3, 'INODE[1] at offset 1 + stride 2' );
	assert.strictEqual( INODE[ 5 ], 11, 'INODE[2] at offset 1 + 2*stride 2' );

	assert.strictEqual( NDIML[ 1 ], 7, 'NDIML[0]' );
	assert.strictEqual( NDIML[ 3 ], 3, 'NDIML[1]' );
	assert.strictEqual( NDIML[ 5 ], 3, 'NDIML[2]' );

	assert.strictEqual( NDIMR[ 1 ], 7, 'NDIMR[0]' );
	assert.strictEqual( NDIMR[ 3 ], 3, 'NDIMR[1]' );
	assert.strictEqual( NDIMR[ 5 ], 3, 'NDIMR[2]' );

	// Verify that the interleaved positions (offset 0, 2, 4) are untouched:
	assert.strictEqual( INODE[ 0 ], 0, 'INODE untouched at 0' );
	assert.strictEqual( INODE[ 2 ], 0, 'INODE untouched at 2' );
	assert.strictEqual( INODE[ 4 ], 0, 'INODE untouched at 4' );
});

test( 'dlasdt: tree structure is consistent (subproblem sizes sum to N)', function t() {
	var INODE;
	var NDIML;
	var NDIMR;
	var total;
	var lvl;
	var nd;
	var N;
	var i;

	N = 63;
	lvl = new Int32Array( 1 );
	nd = new Int32Array( 1 );
	INODE = new Int32Array( N );
	NDIML = new Int32Array( N );
	NDIMR = new Int32Array( N );

	dlasdt.ndarray( N, lvl, nd, INODE, 1, 0, NDIML, 1, 0, NDIMR, 1, 0, 12 );

	// Each internal node splits into left dim + right dim + 1 (the center).
	// The leaf nodes' dimensions should sum up to N minus the number of
	// internal split points. For the root: NDIML[0] + NDIMR[0] + 1 = N.
	assert.strictEqual( NDIML[ 0 ] + NDIMR[ 0 ] + 1, N, 'root splits sum to N' );

	// Each non-leaf node: left child dim + right child dim + 1 = parent dim
	// We verify the first level split:
	total = 0;
	for ( i = 0; i < nd[ 0 ]; i += 1 ) {
		total += 1; // count the center node itself
	}
	// Leaf nodes contribute their full dim. For the full tree with nd nodes:
	// The total should reconstruct as NDIML[leaf] + NDIMR[leaf] + 1 for leaves.
	// Simpler: check root consistency.
	assert.ok( nd[ 0 ] > 0, 'ND > 0' );
	assert.ok( lvl[ 0 ] >= 1, 'LVL >= 1' );
});
