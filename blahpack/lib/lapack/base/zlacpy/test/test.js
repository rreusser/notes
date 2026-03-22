'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlacpy = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlacpy.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}

/**
* Builds a clean input array from the fixture, zeroing out non-data positions.
*
* In the Fortran fixtures, the arrays have LDA=5 rows, but M<5, so positions
* beyond the first M rows contain garbage. For JS tests we only care about
* the M active rows per column, so we build a clean array.
*
* @private
* @param {Array} fixtureArr - raw fixture data (doubles)
* @param {number} M - number of active rows
* @param {number} N - number of columns
* @param {number} LDA - leading dimension (rows allocated)
* @returns {Float64Array} clean array
*/
function buildInput( fixtureArr, M, N, LDA ) {
	var out = new Float64Array( 2 * LDA * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out[ j * 2 * LDA + i * 2 ] = fixtureArr[ j * 2 * LDA + i * 2 ];
			out[ j * 2 * LDA + i * 2 + 1 ] = fixtureArr[ j * 2 * LDA + i * 2 + 1 ];
		}
	}
	return out;
}


// TESTS //

test( 'zlacpy: full copy 3x3', function t() {
	var tc = findCase( 'full_copy_3x3' );
	var LDA = 5;
	var A = buildInput( tc.A, tc.M, tc.N, LDA );
	var B = new Float64Array( 2 * LDA * tc.N );

	zlacpy( 'A', tc.M, tc.N, A, 2, 2 * LDA, 0, B, 2, 2 * LDA, 0 );

	assertArrayClose( Array.from( B ), tc.B, 1e-14, 'B' );
});

test( 'zlacpy: upper copy 3x3', function t() {
	var tc = findCase( 'upper_copy_3x3' );
	var LDA = 5;
	var A = buildInput( tc.A, tc.M, tc.N, LDA );
	var B = new Float64Array( 2 * LDA * tc.N );

	zlacpy( 'U', tc.M, tc.N, A, 2, 2 * LDA, 0, B, 2, 2 * LDA, 0 );

	assertArrayClose( Array.from( B ), tc.B, 1e-14, 'B' );
});

test( 'zlacpy: lower copy 3x3', function t() {
	var tc = findCase( 'lower_copy_3x3' );
	var LDA = 5;
	var A = buildInput( tc.A, tc.M, tc.N, LDA );
	var B = new Float64Array( 2 * LDA * tc.N );

	zlacpy( 'L', tc.M, tc.N, A, 2, 2 * LDA, 0, B, 2, 2 * LDA, 0 );

	assertArrayClose( Array.from( B ), tc.B, 1e-14, 'B' );
});

test( 'zlacpy: M=0 returns B unchanged', function t() {
	var B = new Float64Array( 30 );
	zlacpy( 'A', 0, 3, new Float64Array( 30 ), 2, 10, 0, B, 2, 10, 0 );

	var expected = new Float64Array( 30 );
	assertArrayClose( Array.from( B ), Array.from( expected ), 1e-14, 'B' );
});

test( 'zlacpy: N=0 returns B unchanged', function t() {
	var B = new Float64Array( 30 );
	zlacpy( 'A', 3, 0, new Float64Array( 30 ), 2, 10, 0, B, 2, 10, 0 );

	var expected = new Float64Array( 30 );
	assertArrayClose( Array.from( B ), Array.from( expected ), 1e-14, 'B' );
});

test( 'zlacpy: full copy 2x4 (non-square)', function t() {
	var tc = findCase( 'full_copy_2x4' );
	var LDA = 5;
	var A = buildInput( tc.A, tc.M, tc.N, LDA );
	var B = new Float64Array( 2 * LDA * tc.N );

	zlacpy( 'A', tc.M, tc.N, A, 2, 2 * LDA, 0, B, 2, 2 * LDA, 0 );

	assertArrayClose( Array.from( B ), tc.B, 1e-14, 'B' );
});

test( 'zlacpy: upper copy 4x2 (non-square, M > N)', function t() {
	var tc = findCase( 'upper_copy_4x2' );
	var LDA = 5;
	var A = buildInput( tc.A, tc.M, tc.N, LDA );
	var B = new Float64Array( 2 * LDA * tc.N );

	zlacpy( 'U', tc.M, tc.N, A, 2, 2 * LDA, 0, B, 2, 2 * LDA, 0 );

	assertArrayClose( Array.from( B ), tc.B, 1e-14, 'B' );
});

test( 'zlacpy: lower copy 4x2 (non-square, M > N)', function t() {
	var tc = findCase( 'lower_copy_4x2' );
	var LDA = 5;
	var A = buildInput( tc.A, tc.M, tc.N, LDA );
	var B = new Float64Array( 2 * LDA * tc.N );

	zlacpy( 'L', tc.M, tc.N, A, 2, 2 * LDA, 0, B, 2, 2 * LDA, 0 );

	assertArrayClose( Array.from( B ), tc.B, 1e-14, 'B' );
});

test( 'zlacpy: returns B', function t() {
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var B = new Float64Array( 4 );
	var out = zlacpy( 'A', 1, 1, A, 2, 2, 0, B, 2, 2, 0 );
	assert.strictEqual( out, B );
});

test( 'zlacpy: supports lowercase uplo', function t() {
	// A 2x2 complex matrix, col-major with LDA=2
	// [ (1+2i)  (5+6i) ]
	// [ (3+4i)  (7+8i) ]
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var B;

	// Test lowercase 'u'
	B = new Float64Array( 8 );
	zlacpy( 'u', 2, 2, A, 2, 4, 0, B, 2, 4, 0 );
	// Upper triangle: (0,0)=(1,2), (0,1)=(5,6), (1,1)=(7,8)
	assert.deepStrictEqual( Array.from( B ), [ 1, 2, 0, 0, 5, 6, 7, 8 ] );

	// Test lowercase 'l'
	B = new Float64Array( 8 );
	zlacpy( 'l', 2, 2, A, 2, 4, 0, B, 2, 4, 0 );
	// Lower triangle: (0,0)=(1,2), (1,0)=(3,4), (1,1)=(7,8)
	assert.deepStrictEqual( Array.from( B ), [ 1, 2, 3, 4, 0, 0, 7, 8 ] );
});

test( 'zlacpy: supports offset', function t() {
	// Place a 1x1 complex matrix at offset 4 in a larger array
	var A = new Float64Array( [ 0, 0, 0, 0, 99, 42 ] );
	var B = new Float64Array( [ 0, 0, 0, 0, 0, 0 ] );

	zlacpy( 'A', 1, 1, A, 2, 2, 4, B, 2, 2, 4 );

	assert.deepStrictEqual( Array.from( B ), [ 0, 0, 0, 0, 99, 42 ] );
});

test( 'zlacpy: M=0 with upper uplo returns B unchanged', function t() {
	var B = new Float64Array( 8 );
	zlacpy( 'U', 0, 2, new Float64Array( 8 ), 2, 4, 0, B, 2, 4, 0 );
	assert.deepStrictEqual( Array.from( B ), [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
});

test( 'zlacpy: M=0 with lower uplo returns B unchanged', function t() {
	var B = new Float64Array( 8 );
	zlacpy( 'L', 0, 2, new Float64Array( 8 ), 2, 4, 0, B, 2, 4, 0 );
	assert.deepStrictEqual( Array.from( B ), [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
});

test( 'zlacpy: N=0 with upper uplo returns B unchanged', function t() {
	var B = new Float64Array( 8 );
	zlacpy( 'U', 2, 0, new Float64Array( 8 ), 2, 4, 0, B, 2, 4, 0 );
	assert.deepStrictEqual( Array.from( B ), [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
});

test( 'zlacpy: N=0 with lower uplo returns B unchanged', function t() {
	var B = new Float64Array( 8 );
	zlacpy( 'L', 2, 0, new Float64Array( 8 ), 2, 4, 0, B, 2, 4, 0 );
	assert.deepStrictEqual( Array.from( B ), [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
});
