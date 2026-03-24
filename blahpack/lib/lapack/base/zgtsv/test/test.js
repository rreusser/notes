

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgtsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgtsv.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Creates a Complex128Array from interleaved re/im data.
*/
function c128( data ) {
	return new Complex128Array( new Float64Array( data ) );
}


// TESTS //

test( 'zgtsv: basic_5x5_single_rhs', function t() {
	var tc = findCase( 'basic_5x5_single_rhs' );
	var N = 5;
	var nrhs = 1;
	var DL = c128( [ -1, 0, -1, 0, -1, 0, -1, 0 ] );
	var d = c128( [ 2, 0, 2, 0, 2, 0, 2, 0, 2, 0 ] );
	var DU = c128( [ -1, 0, -1, 0, -1, 0, -1, 0 ] );
	var B = c128( [ 1, 0, 2, 0, 3, 0, 4, 0, 5, 0 ] );
	var info;
	var bv;
	var dv;

	info = zgtsv( N, nrhs, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, N, 0 );

	assert.equal( info, tc.info );
	dv = reinterpret( d, 0 );
	assertArrayClose( Array.from( dv ), tc.d, 1e-14, 'd' );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zgtsv: multi_rhs_complex', function t() {
	var tc = findCase( 'multi_rhs_complex' );
	var N = 4;
	var nrhs = 2;
	// DL: (1+i), (1-i), (2+0i)
	var DL = c128( [ 1, 1, 1, -1, 2, 0 ] );
	// d: (4+0i), (4+i), (4-i), (4+0i)
	var d = c128( [ 4, 0, 4, 1, 4, -1, 4, 0 ] );
	// DU: (1-i), (1+i), (1+0i)
	var DU = c128( [ 1, -1, 1, 1, 1, 0 ] );
	// B: 4 rows x 2 cols, column-major in complex elements
	// B(1:4, 1) then B(1:4, 2)
	var B = c128( [
		6, -1, 10, 1, 10, -1, 7, 0,
		2, 0, 3, 1, 3, -1, 2, 0
	] );
	var info;
	var bv;
	var dv;

	info = zgtsv( N, nrhs, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, N, 0 );

	assert.equal( info, tc.info );
	dv = reinterpret( d, 0 );
	assertArrayClose( Array.from( dv ), tc.d, 1e-14, 'd' );

	// Check b: first 8 doubles = column 1, next 8 = column 2
	bv = reinterpret( B, 0 );
	var bCol1 = Array.from( bv ).slice( 0, 2 * N );
	var bCol2 = Array.from( bv ).slice( 2 * N, 4 * N );
	assertArrayClose( bCol1, tc.b.slice( 0, 2 * N ), 1e-13, 'b col1' );
	assertArrayClose( bCol2, tc.b.slice( 2 * N, 4 * N ), 1e-13, 'b col2' );
});

test( 'zgtsv: n_one', function t() {
	var tc = findCase( 'n_one' );
	var d = c128( [ 5, 2 ] );
	var DL = c128( [] );
	var DU = c128( [] );
	var B = c128( [ 10, 4 ] );
	var info;
	var bv;

	info = zgtsv( 1, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-14, 'b' );
});

test( 'zgtsv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var d = c128( [] );
	var DL = c128( [] );
	var DU = c128( [] );
	var B = c128( [] );
	var info;

	info = zgtsv( 0, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, 0, 0 );

	assert.equal( info, tc.info );
});

test( 'zgtsv: singular', function t() {
	var tc = findCase( 'singular' );
	var DL = c128( [ 0, 0, 0, 0 ] );
	var d = c128( [ 0, 0, 2, 0, 3, 0 ] );
	var DU = c128( [ 1, 0, 1, 0 ] );
	var B = c128( [ 1, 0, 2, 0, 3, 0 ] );
	var info;

	info = zgtsv( 3, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info );
});

test( 'zgtsv: pivoting', function t() {
	var tc = findCase( 'pivoting' );
	var N = 4;
	var DL = c128( [ 5, 0, 7, 0, 9, 0 ] );
	var d = c128( [ 1, 0, 3, 0, 2, 0, 1, 0 ] );
	var DU = c128( [ 2, 0, 4, 0, 6, 0 ] );
	var B = c128( [ 5, 0, 12, 0, 15, 0, 10, 0 ] );
	var info;
	var bv;
	var dv;
	var dlv;
	var duv;

	info = zgtsv( N, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, N, 0 );

	assert.equal( info, tc.info );
	dv = reinterpret( d, 0 );
	assertArrayClose( Array.from( dv ), tc.d, 1e-14, 'd' );
	dlv = reinterpret( DL, 0 );
	assertArrayClose( Array.from( dlv ), tc.dl, 1e-14, 'dl' );
	duv = reinterpret( DU, 0 );
	assertArrayClose( Array.from( duv ), tc.du, 1e-14, 'du' );
	bv = reinterpret( B, 0 );
	assertArrayClose( Array.from( bv ), tc.b, 1e-13, 'b' );
});
