'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgetrf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgetrf2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgetrf2: 3x3 non-singular matrix', function t() {
	var tc = findCase( '3x3' );
	// A = [2 1 1; 4 3 3; 8 7 9] col-major
	var a = new Float64Array( [ 2, 4, 8, 1, 3, 7, 1, 3, 9 ] );
	var ipiv = new Int32Array( 3 );
	var info = dgetrf2( 3, 3, a, 1, 3, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	var expectedIPIV = ipivTo0Based( tc.ipiv );
	assert.deepEqual( ipiv, expectedIPIV, 'ipiv' );
});

test( 'dgetrf2: 4x3 tall matrix', function t() {
	var tc = findCase( '4x3' );
	// A = [2 1 0; 0 3 1; 1 0 4; 0 1 2] col-major
	var a = new Float64Array( [ 2, 0, 1, 0, 1, 3, 0, 1, 0, 1, 4, 2 ] );
	var ipiv = new Int32Array( 3 );
	var info = dgetrf2( 4, 3, a, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: 3x4 wide matrix', function t() {
	var tc = findCase( '3x4' );
	// A = [1 2 3 10; 4 5 6 11; 7 8 9 12] col-major
	var a = new Float64Array( [ 1, 4, 7, 2, 5, 8, 3, 6, 9, 10, 11, 12 ] );
	var ipiv = new Int32Array( 3 );
	var info = dgetrf2( 3, 4, a, 1, 3, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: singular matrix (info > 0)', function t() {
	var tc = findCase( 'singular' );
	// A = [1 0 0; 0 0 0; 0 0 1] col-major — zero at (2,2)
	var a = new Float64Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1 ] );
	var ipiv = new Int32Array( 3 );
	var info = dgetrf2( 3, 3, a, 1, 3, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info should be 2 (singular at column 2)' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: N=0 quick return', function t() {
	var a = new Float64Array( [ 99 ] );
	var ipiv = new Int32Array( 1 );
	var info = dgetrf2( 3, 0, a, 1, 3, 0, ipiv, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dgetrf2: M=0 quick return', function t() {
	var a = new Float64Array( 1 );
	var ipiv = new Int32Array( 1 );
	var info = dgetrf2( 0, 3, a, 1, 1, 0, ipiv, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dgetrf2: 1x1 non-singular', function t() {
	var tc = findCase( '1x1' );
	var a = new Float64Array( [ 5 ] );
	var ipiv = new Int32Array( 1 );
	var info = dgetrf2( 1, 1, a, 1, 1, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: 1x1 singular (zero)', function t() {
	var a = new Float64Array( [ 0 ] );
	var ipiv = new Int32Array( 1 );
	var info = dgetrf2( 1, 1, a, 1, 1, 0, ipiv, 1, 0 );
	assert.equal( a[ 0 ], 0.0, 'a[0] should be 0' );
	assert.equal( info, 1, 'info should be 1' );
	assert.equal( ipiv[ 0 ], 0, 'ipiv[0] should be 0 (0-based)' );
});

test( 'dgetrf2: Nx1 column vector', function t() {
	var tc = findCase( 'col_vector' );
	var a = new Float64Array( [ 1, 5, 3 ] );
	var ipiv = new Int32Array( 1 );
	var info = dgetrf2( 3, 1, a, 1, 3, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: 1xN row vector', function t() {
	var tc = findCase( 'row_vector' );
	var a = new Float64Array( [ 2, 3, 7 ] );
	var ipiv = new Int32Array( 1 );
	var info = dgetrf2( 1, 3, a, 1, 1, 0, ipiv, 1, 0 );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( ipiv, ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf2: Nx1 column with subnormal pivot (sfmin path)', function t() {
	// Create a column vector where the pivot value is below sfmin (~2.2e-308).
	// This triggers the element-wise division loop instead of dscal.
	var a = new Float64Array( [ 5e-324, 1.0, 2.0 ] );
	var ipiv = new Int32Array( 1 );
	var info = dgetrf2( 3, 1, a, 1, 3, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'info should be 0' );
	// The pivot is row 1 (0-based: row with value 2.0, found by idamax)
	// After swap, a[0]=2.0, then a[1] and a[2] are scaled by 1/2.0
	// But since the pivot was at index 2 (idamax of [5e-324, 1.0, 2.0] = 2),
	// after swap: a = [2.0, 1.0, 5e-324]
	// Since abs(2.0) >= sfmin, dscal is used. Let's use a truly tiny pivot instead.

	// Retry: Make largest element be very tiny so pivot itself is tiny
	var a2 = new Float64Array( [ 1e-310, 5e-311, 2e-311 ] );
	var ipiv2 = new Int32Array( 1 );
	var info2 = dgetrf2( 3, 1, a2, 1, 3, 0, ipiv2, 1, 0 );
	assert.equal( info2, 0, 'info should be 0' );
	assert.equal( ipiv2[ 0 ], 0, 'pivot should be row 0 (largest element)' );
	// Since abs(a2[0]) = 1e-310 < sfmin ≈ 2.2e-308, element-wise division is used
	assertClose( a2[ 1 ], 5e-311 / 1e-310, 1e-10, 'a2[1]' );
	assertClose( a2[ 2 ], 2e-311 / 1e-310, 1e-10, 'a2[2]' );
});
