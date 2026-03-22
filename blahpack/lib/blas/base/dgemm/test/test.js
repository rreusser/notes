'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgemm = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgemm.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// All matrices stored column-major: strideA1=1, strideA2=LDA (number of rows)

test( 'dgemm: basic N,N 2x2', function t() {
	var tc = findCase( 'basic_nn' );
	// A = [1 3; 2 4] col-major, B = [5 7; 6 8] col-major
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var B = new Float64Array( [ 5, 6, 7, 8 ] );
	var C = new Float64Array( 4 );
	dgemm( 'N', 'N', 2, 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'basic_nn' );
});

test( 'dgemm: T,N transpose A', function t() {
	var tc = findCase( 'tn' );
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var B = new Float64Array( [ 5, 6, 7, 8 ] );
	var C = new Float64Array( 4 );
	dgemm( 'T', 'N', 2, 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'tn' );
});

test( 'dgemm: N,T transpose B', function t() {
	var tc = findCase( 'nt' );
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var B = new Float64Array( [ 5, 7, 6, 8 ] );
	var C = new Float64Array( 4 );
	dgemm( 'N', 'T', 2, 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'nt' );
});

test( 'dgemm: alpha=0 just scales C by beta', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var B = new Float64Array( [ 5, 7, 6, 8 ] );
	var C = new Float64Array( [ 1, 2, 3, 4 ] );
	dgemm( 'N', 'N', 2, 2, 2, 0.0, A, 1, 2, 0, B, 1, 2, 0, 2.0, C, 1, 2, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'alpha_zero' );
});

test( 'dgemm: beta=0 overwrites C', function t() {
	var tc = findCase( 'beta_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var B = new Float64Array( [ 5, 6, 7, 8 ] );
	var C = new Float64Array( [ 999, 999, 999, 999 ] );
	dgemm( 'N', 'N', 2, 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'beta_zero' );
});

test( 'dgemm: M=0 quick return', function t() {
	var C = new Float64Array( [ 99 ] );
	dgemm( 'N', 'N', 0, 2, 2, 1.0, new Float64Array( 4 ), 1, 1, 0, new Float64Array( 4 ), 1, 2, 0, 0.0, C, 1, 1, 0 );
	// C should be unchanged
	assert.strictEqual( C[ 0 ], 99 );
});

test( 'dgemm: alpha and beta scaling', function t() {
	var tc = findCase( 'alpha_beta' );
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var B = new Float64Array( [ 5, 6, 7, 8 ] );
	var C = new Float64Array( [ 1, 1, 1, 1 ] );
	dgemm( 'N', 'N', 2, 2, 2, 2.0, A, 1, 2, 0, B, 1, 2, 0, 3.0, C, 1, 2, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'alpha_beta' );
});

test( 'dgemm: non-square M=3, N=2, K=2', function t() {
	var tc = findCase( 'nonsquare' );
	// A is 3x2: [1,2,3,4,5,6], B is 2x2: identity [1,0,0,1]
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 1, 0, 0, 1 ] );
	var C = new Float64Array( 6 );
	dgemm( 'N', 'N', 3, 2, 2, 1.0, A, 1, 3, 0, B, 1, 2, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'nonsquare' );
});

test( 'dgemm: T,T both transposed', function t() {
	var tc = findCase( 'tt' );
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var B = new Float64Array( [ 5, 6, 7, 8 ] );
	var C = new Float64Array( 4 );
	dgemm( 'T', 'T', 2, 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'tt' );
});

test( 'dgemm: N=0 quick return', function t() {
	var C = new Float64Array( [ 99 ] );
	dgemm( 'N', 'N', 2, 0, 2, 1.0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, 0.0, C, 1, 2, 0 );
	assert.strictEqual( C[ 0 ], 99 );
});

test( 'dgemm: alpha=0 beta=0 zeros C', function t() {
	var C = new Float64Array( [ 10, 20, 30, 40 ] );
	dgemm( 'N', 'N', 2, 2, 2, 0.0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( C, [ 0, 0, 0, 0 ], 1e-14, 'alpha0_beta0' );
});

test( 'dgemm: K=0 and beta=1 quick return', function t() {
	var C = new Float64Array( [ 10, 20, 30, 40 ] );
	dgemm( 'N', 'N', 2, 2, 0, 1.0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, 1.0, C, 1, 2, 0 );
	assert.strictEqual( C[ 0 ], 10 );
	assert.strictEqual( C[ 1 ], 20 );
});

test( 'dgemm: beta=1 does not scale C', function t() {
	// Test the beta===1.0 branch (no scaling loop)
	var A = new Float64Array( [ 1, 0, 0, 1 ] ); // identity
	var B = new Float64Array( [ 2, 0, 0, 2 ] );
	var C = new Float64Array( [ 1, 1, 1, 1 ] );
	// C = 1.0 * I * B + 1.0 * C = B + C
	dgemm( 'N', 'N', 2, 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 1.0, C, 1, 2, 0 );
	assertArrayClose( C, [ 3, 1, 1, 3 ], 1e-14, 'beta_one' );
});
