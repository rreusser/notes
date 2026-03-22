'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgetrf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgetrf2.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Converts 1-based Fortran IPIV values to 0-based JS IPIV values.
*/
function ipivTo0Based( ipiv1 ) {
	var out = new Int32Array( ipiv1.length );
	var i;
	for ( i = 0; i < ipiv1.length; i++ ) {
		out[ i ] = ipiv1[ i ] - 1;
	}
	return out;
}


// TESTS //

test( 'zgetrf2: 3x3 non-singular complex matrix', function t() {
	var tc = findCase( '3x3' );
	// A = [(2+1i) (1+0.5i) (1+0.1i); (4+2i) (3+1i) (3+0.5i); (8+3i) (7+2i) (9+1i)] col-major
	var a = new Complex128Array( [
		2, 1, 4, 2, 8, 3,
		1, 0.5, 3, 1, 7, 2,
		1, 0.1, 3, 0.5, 9, 1
	] );
	var ipiv = new Int32Array( 3 );
	var info = zgetrf2( 3, 3, a, 1, 3, 0, ipiv, 1, 0 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	var expectedIPIV = ipivTo0Based( tc.ipiv );
	assert.deepEqual( ipiv, expectedIPIV, 'ipiv' );
});

test( 'zgetrf2: 4x3 tall matrix (M > N)', function t() {
	var tc = findCase( '4x3' );
	var a = new Complex128Array( [
		2, 1, 0, 0.5, 1, 0.2, 0, 0.1,
		1, 0.3, 3, 1, 0, 0.4, 1, 0.5,
		0, 0.1, 1, 0.6, 4, 2, 2, 1
	] );
	var ipiv = new Int32Array( 3 );
	var info = zgetrf2( 4, 3, a, 1, 4, 0, ipiv, 1, 0 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf2: 3x4 wide matrix (M < N)', function t() {
	var tc = findCase( '3x4' );
	var a = new Complex128Array( [
		1, 0.5, 4, 1, 7, 2,
		2, 0.3, 5, 1.5, 8, 2.5,
		3, 0.1, 6, 0.5, 9, 3,
		10, 1, 11, 2, 12, 3
	] );
	var ipiv = new Int32Array( 3 );
	var info = zgetrf2( 3, 4, a, 1, 3, 0, ipiv, 1, 0 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf2: singular matrix (info > 0)', function t() {
	var tc = findCase( 'singular' );
	// A = [(1+0i) (0+0i) (0+0i); (0+0i) (0+0i) (0+0i); (0+0i) (0+0i) (1+0i)]
	var a = new Complex128Array( [
		1, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 1, 0
	] );
	var ipiv = new Int32Array( 3 );
	var info = zgetrf2( 3, 3, a, 1, 3, 0, ipiv, 1, 0 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info should be 2 (singular at column 2)' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf2: N=0 quick return', function t() {
	var a = new Complex128Array( [ 99, 99 ] );
	var ipiv = new Int32Array( 1 );
	var info = zgetrf2( 3, 0, a, 1, 3, 0, ipiv, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zgetrf2: M=0 quick return', function t() {
	var a = new Complex128Array( 1 );
	var ipiv = new Int32Array( 1 );
	var info = zgetrf2( 0, 3, a, 1, 1, 0, ipiv, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zgetrf2: 1x1 non-singular', function t() {
	var tc = findCase( '1x1' );
	var a = new Complex128Array( [ 5, 3 ] );
	var ipiv = new Int32Array( 1 );
	var info = zgetrf2( 1, 1, a, 1, 1, 0, ipiv, 1, 0 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf2: 1x1 singular (zero)', function t() {
	var a = new Complex128Array( [ 0, 0 ] );
	var ipiv = new Int32Array( 1 );
	var info = zgetrf2( 1, 1, a, 1, 1, 0, ipiv, 1, 0 );
	var view = reinterpret( a, 0 );
	assert.equal( view[ 0 ], 0.0, 'a[0] should be 0' );
	assert.equal( view[ 1 ], 0.0, 'a[1] should be 0' );
	assert.equal( info, 1, 'info should be 1' );
	assert.equal( ipiv[ 0 ], 0, 'ipiv[0] should be 0 (0-based)' );
});

test( 'zgetrf2: Nx1 column vector', function t() {
	var tc = findCase( 'col_vector' );
	var a = new Complex128Array( [ 1, 0.5, 5, 2, 3, 1 ] );
	var ipiv = new Int32Array( 1 );
	var info = zgetrf2( 3, 1, a, 1, 3, 0, ipiv, 1, 0 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf2: 1xN row vector', function t() {
	var tc = findCase( 'row_vector' );
	var a = new Complex128Array( [ 2, 1, 3, 0.5, 7, 2 ] );
	var ipiv = new Int32Array( 1 );
	var info = zgetrf2( 1, 3, a, 1, 1, 0, ipiv, 1, 0 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf2: 4x4 complex matrix', function t() {
	var tc = findCase( '4x4' );
	var a = new Complex128Array( [
		1, 2, 5, 6, 9, 10, 13, 14,
		2, 3, 6, 7, 10, 11, 14, 15,
		3, 1, 7, 2, 11, 3, 15, 4,
		4, 0.5, 8, 1.5, 12, 2.5, 16, 3.5
	] );
	var ipiv = new Int32Array( 4 );
	var info = zgetrf2( 4, 4, a, 1, 4, 0, ipiv, 1, 0 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-13, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf2: Nx1 column with subnormal pivot (sfmin path)', function t() {
	// Create a column vector where the pivot value is below sfmin (~2.2e-308)
	// This triggers the element-wise division loop instead of zscal.
	var a = new Complex128Array( [ 1e-310, 0, 5e-311, 0, 2e-311, 0 ] );
	var ipiv = new Int32Array( 1 );
	var info = zgetrf2( 3, 1, a, 1, 3, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'info should be 0' );
	assert.equal( ipiv[ 0 ], 0, 'pivot should be row 0 (largest element)' );
	// Since |a[0]| = 1e-310 < sfmin ~= 2.2e-308, element-wise division is used
	var view = reinterpret( a, 0 );
	assertClose( view[ 2 ], 5e-311 / 1e-310, 1e-10, 'a[1] real' );
	assertClose( view[ 4 ], 2e-311 / 1e-310, 1e-10, 'a[2] real' );
});
