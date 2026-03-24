

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dptts2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dptts2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dptts2: basic_5x5_single_rhs', function t() {
	var tc = findCase( 'basic_5x5_single_rhs' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 3.0, 4.0 ] );
	var e = new Float64Array( [ 0.5, -0.5, 0.25, -0.25 ] );
	var B = new Float64Array( [ 8.0, 5.5, 7.25, 10.25, 17.9375 ] );

	// N=5, NRHS=1, strideB1=1, strideB2=5 (col-major, LDB=5)
	dptts2( 5, 1, d, 1, 0, e, 1, 0, B, 1, 5, 0 );

	assertArrayClose( B, tc.x, 1e-14, 'x' );
});

test( 'dptts2: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 3.0, 4.0 ] );
	var e = new Float64Array( [ 0.5, -0.5, 0.25, -0.25 ] );
	// B is 5x2, col-major (LDB=5): col1 at [0..4], col2 at [5..9]
	var B = new Float64Array( [
		8.0, 5.5, 7.25, 10.25, 17.9375,
		28.0, 21.5, 3.25, 7.0, 2.6875
	] );

	dptts2( 5, 2, d, 1, 0, e, 1, 0, B, 1, 5, 0 );

	assertArrayClose( B, tc.x, 1e-14, 'x' );
});

test( 'dptts2: n_eq_1', function t() {
	var tc = findCase( 'n_eq_1' );
	var d = new Float64Array( [ 3.0 ] );
	var e = new Float64Array( [] );
	var B = new Float64Array( [ 9.0 ] );

	dptts2( 1, 1, d, 1, 0, e, 1, 0, B, 1, 1, 0 );

	assertArrayClose( B, tc.x, 1e-14, 'x' );
});

test( 'dptts2: n_eq_1_multi_rhs', function t() {
	var tc = findCase( 'n_eq_1_multi_rhs' );
	var d = new Float64Array( [ 4.0 ] );
	var e = new Float64Array( [] );
	// B is 1x2, col-major (LDB=1): [8, 12]
	var B = new Float64Array( [ 8.0, 12.0 ] );

	dptts2( 1, 2, d, 1, 0, e, 1, 0, B, 1, 1, 0 );

	assertArrayClose( B, tc.x, 1e-14, 'x' );
});

test( 'dptts2: n_eq_0', function t() {
	var tc = findCase( 'n_eq_0' );
	var d = new Float64Array( [ 4.0 ] );
	var e = new Float64Array( [] );
	var B = new Float64Array( [ 42.0 ] );

	dptts2( 0, 1, d, 1, 0, e, 1, 0, B, 1, 1, 0 );

	assertArrayClose( B, tc.x, 1e-14, 'x' );
});

test( 'dptts2: returns B', function t() {
	var d = new Float64Array( [ 2.0 ] );
	var e = new Float64Array( [] );
	var B = new Float64Array( [ 6.0 ] );
	var result = dptts2( 1, 1, d, 1, 0, e, 1, 0, B, 1, 1, 0 );

	assert.equal( result, B );
});

test( 'dptts2: supports offsets', function t() {
	// Put actual data starting at offset 2 for d, 1 for e, 3 for B
	var d = new Float64Array( [ 0.0, 0.0, 4.0, 3.0 ] );
	var e = new Float64Array( [ 0.0, 0.5 ] );
	// 2x2 system: D=[4,3], E=[0.5]
	// A diagonal: [4, 3+0.25*4] = [4, 4], off-diag: [0.5*4] = [2]
	// x=[1,2], b=[4+4, 2+8]=[8, 10]
	var B = new Float64Array( [ 0.0, 0.0, 0.0, 8.0, 10.0 ] );

	dptts2( 2, 1, d, 1, 2, e, 1, 1, B, 1, 2, 3 );

	assertClose( B[ 3 ], 1.0, 1e-14, 'x[0]' );
	assertClose( B[ 4 ], 2.0, 1e-14, 'x[1]' );
});

test( 'dptts2: supports strides', function t() {
	// D with stride 2: [4, ?, 3]
	var d = new Float64Array( [ 4.0, 999.0, 3.0 ] );
	// E with stride 2: [0.5]
	var e = new Float64Array( [ 0.5 ] );
	// 2x1, B with strideB1=2 (non-contiguous rows)
	// Same system as offset test: b=[8, 10]
	var B = new Float64Array( [ 8.0, 999.0, 10.0 ] );

	dptts2( 2, 1, d, 2, 0, e, 1, 0, B, 2, 4, 0 );

	assertClose( B[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( B[ 2 ], 2.0, 1e-14, 'x[1]' );
});
