'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlasyf = require( './../lib/base.js' );


// FIXTURES //

var MAXN = 10; // must match Fortran test LDA
var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlasyf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

/**
* Extract NxN complex matrix from fixture with leading dimension MAXN.
* Fixture stores column-major with LDA=MAXN as flat Float64 pairs.
* Returns flat Float64 array with LDA=N.
*/
function extractA( fixtureA, N ) {
	var result = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			result.push( fixtureA[ (i + j * MAXN) * 2 ] );
			result.push( fixtureA[ (i + j * MAXN) * 2 + 1 ] );
		}
	}
	return result;
}

/**
* Compare two Float64 arrays element by element with tolerance.
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch: actual ' + actual.length + ' vs expected ' + expected.length );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Convert Fortran 1-based IPIV to expected JS 0-based IPIV.
* Positive Fortran k -> JS k-1; negative Fortran -k -> JS ~(k-1) = -k (same value).
*/
function fortranIPIVtoJS( ipiv ) {
	return ipiv.map( function convert( v ) {
		if ( v > 0 ) {
			return v - 1;
		}
		return v; // negative values: ~(k-1) = -k, same as Fortran
	});
}


// TESTS //

test( 'zlasyf: main export is a function', function t() {
	assert.strictEqual( typeof zlasyf, 'function' );
});

test( 'zlasyf: 4x4 upper, nb=4', function t() {
	var tc = findCase( 'upper_4x4' );
	var N = 4;
	var nb = 4;
	var A = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var expectedA;
	var expected;
	var result;
	var i;

	// Fill upper triangle of symmetric matrix (column-major)
	Av[ (0 + 0 * N) * 2 ] = 4; Av[ (0 + 0 * N) * 2 + 1 ] = 1;
	Av[ (0 + 1 * N) * 2 ] = 1; Av[ (0 + 1 * N) * 2 + 1 ] = 2;
	Av[ (1 + 1 * N) * 2 ] = 5; Av[ (1 + 1 * N) * 2 + 1 ] = 0;
	Av[ (0 + 2 * N) * 2 ] = 2; Av[ (0 + 2 * N) * 2 + 1 ] = 0;
	Av[ (1 + 2 * N) * 2 ] = 1; Av[ (1 + 2 * N) * 2 + 1 ] = 1;
	Av[ (2 + 2 * N) * 2 ] = 6; Av[ (2 + 2 * N) * 2 + 1 ] = 2;
	Av[ (0 + 3 * N) * 2 ] = 0; Av[ (0 + 3 * N) * 2 + 1 ] = 1;
	Av[ (1 + 3 * N) * 2 ] = 0; Av[ (1 + 3 * N) * 2 + 1 ] = 0;
	Av[ (2 + 3 * N) * 2 ] = 1; Av[ (2 + 3 * N) * 2 + 1 ] = -1;
	Av[ (3 + 3 * N) * 2 ] = 3; Av[ (3 + 3 * N) * 2 + 1 ] = 0;

	var W = new Complex128Array( N * nb );
	var IPIV = new Int32Array( N );
	result = zlasyf( 'U', N, nb, A, 1, N, 0, IPIV, 1, 0, W, 1, N, 0 );
	assert.strictEqual( result.info, tc.info );
	assert.strictEqual( result.kb, tc.kb );
	expectedA = extractA( tc.A, N );
	assertArrayClose( Array.from( Av ), expectedA, 1e-10, 'A' );
	expected = fortranIPIVtoJS( tc.ipiv );
	for ( i = 0; i < N; i++ ) {
		assert.strictEqual( IPIV[ i ], expected[ i ], 'ipiv[' + i + ']' );
	}
});

test( 'zlasyf: 4x4 lower, nb=4', function t() {
	var tc = findCase( 'lower_4x4' );
	var N = 4;
	var nb = 4;
	var A = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var expectedA;
	var expected;
	var result;
	var i;

	// Fill lower triangle
	Av[ (0 + 0 * N) * 2 ] = 4; Av[ (0 + 0 * N) * 2 + 1 ] = 1;
	Av[ (1 + 0 * N) * 2 ] = 1; Av[ (1 + 0 * N) * 2 + 1 ] = 2;
	Av[ (2 + 0 * N) * 2 ] = 2; Av[ (2 + 0 * N) * 2 + 1 ] = 0;
	Av[ (3 + 0 * N) * 2 ] = 0; Av[ (3 + 0 * N) * 2 + 1 ] = 1;
	Av[ (1 + 1 * N) * 2 ] = 5; Av[ (1 + 1 * N) * 2 + 1 ] = 0;
	Av[ (2 + 1 * N) * 2 ] = 1; Av[ (2 + 1 * N) * 2 + 1 ] = 1;
	Av[ (3 + 1 * N) * 2 ] = 0; Av[ (3 + 1 * N) * 2 + 1 ] = 0;
	Av[ (2 + 2 * N) * 2 ] = 6; Av[ (2 + 2 * N) * 2 + 1 ] = 2;
	Av[ (3 + 2 * N) * 2 ] = 1; Av[ (3 + 2 * N) * 2 + 1 ] = -1;
	Av[ (3 + 3 * N) * 2 ] = 3; Av[ (3 + 3 * N) * 2 + 1 ] = 0;

	var W = new Complex128Array( N * nb );
	var IPIV = new Int32Array( N );
	result = zlasyf( 'L', N, nb, A, 1, N, 0, IPIV, 1, 0, W, 1, N, 0 );
	assert.strictEqual( result.info, tc.info );
	assert.strictEqual( result.kb, tc.kb );
	expectedA = extractA( tc.A, N );
	assertArrayClose( Array.from( Av ), expectedA, 1e-10, 'A' );
	expected = fortranIPIVtoJS( tc.ipiv );
	for ( i = 0; i < N; i++ ) {
		assert.strictEqual( IPIV[ i ], expected[ i ], 'ipiv[' + i + ']' );
	}
});

test( 'zlasyf: 3x3 upper, nb=2 (partial panel)', function t() {
	var tc = findCase( 'upper_3x3_nb2' );
	var N = 3;
	var nb = 2;
	var A = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var expectedA;
	var expected;
	var result;
	var i;

	Av[ (0 + 0 * N) * 2 ] = 5; Av[ (0 + 0 * N) * 2 + 1 ] = 0;
	Av[ (0 + 1 * N) * 2 ] = 2; Av[ (0 + 1 * N) * 2 + 1 ] = 1;
	Av[ (1 + 1 * N) * 2 ] = 4; Av[ (1 + 1 * N) * 2 + 1 ] = 0;
	Av[ (0 + 2 * N) * 2 ] = 1; Av[ (0 + 2 * N) * 2 + 1 ] = 0;
	Av[ (1 + 2 * N) * 2 ] = 1; Av[ (1 + 2 * N) * 2 + 1 ] = -1;
	Av[ (2 + 2 * N) * 2 ] = 6; Av[ (2 + 2 * N) * 2 + 1 ] = 1;

	var W = new Complex128Array( N * nb );
	var IPIV = new Int32Array( N );
	result = zlasyf( 'U', N, nb, A, 1, N, 0, IPIV, 1, 0, W, 1, N, 0 );
	assert.strictEqual( result.info, tc.info );
	assert.strictEqual( result.kb, tc.kb );
	expectedA = extractA( tc.A, N );
	assertArrayClose( Array.from( Av ), expectedA, 1e-10, 'A' );
	expected = fortranIPIVtoJS( tc.ipiv );
	for ( i = 0; i < N; i++ ) {
		assert.strictEqual( IPIV[ i ], expected[ i ], 'ipiv[' + i + ']' );
	}
});

test( 'zlasyf: 1x1 upper', function t() {
	var tc = findCase( 'upper_1x1' );
	var expectedA;
	var result;

	var A = new Complex128Array( [ 7, 3 ] );
	var W = new Complex128Array( 1 );
	var IPIV = new Int32Array( 1 );
	result = zlasyf( 'U', 1, 1, A, 1, 1, 0, IPIV, 1, 0, W, 1, 1, 0 );
	assert.strictEqual( result.info, tc.info );
	assert.strictEqual( result.kb, tc.kb );
	expectedA = extractA( tc.A, 1 );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), expectedA, 1e-10, 'A' );
	assert.strictEqual( IPIV[ 0 ], fortranIPIVtoJS( tc.ipiv )[ 0 ], 'ipiv[0]' );
});

test( 'zlasyf: 2x2 upper with pivot', function t() {
	var tc = findCase( 'upper_2x2_pivot' );
	var N = 2;
	var A = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var expectedA;
	var expected;
	var result;
	var i;

	Av[ (0 + 0 * N) * 2 ] = 0.1; Av[ (0 + 0 * N) * 2 + 1 ] = 0;
	Av[ (0 + 1 * N) * 2 ] = 3; Av[ (0 + 1 * N) * 2 + 1 ] = 4;
	Av[ (1 + 1 * N) * 2 ] = 10; Av[ (1 + 1 * N) * 2 + 1 ] = 0;

	var W = new Complex128Array( N * 2 );
	var IPIV = new Int32Array( N );
	result = zlasyf( 'U', N, 2, A, 1, N, 0, IPIV, 1, 0, W, 1, N, 0 );
	assert.strictEqual( result.info, tc.info );
	assert.strictEqual( result.kb, tc.kb );
	expectedA = extractA( tc.A, N );
	assertArrayClose( Array.from( Av ), expectedA, 1e-10, 'A' );
	expected = fortranIPIVtoJS( tc.ipiv );
	for ( i = 0; i < N; i++ ) {
		assert.strictEqual( IPIV[ i ], expected[ i ], 'ipiv[' + i + ']' );
	}
});

test( 'zlasyf: 3x3 lower', function t() {
	var tc = findCase( 'lower_3x3' );
	var N = 3;
	var nb = 3;
	var A = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var expectedA;
	var expected;
	var result;
	var i;

	Av[ (0 + 0 * N) * 2 ] = 5; Av[ (0 + 0 * N) * 2 + 1 ] = 0;
	Av[ (1 + 0 * N) * 2 ] = 2; Av[ (1 + 0 * N) * 2 + 1 ] = 1;
	Av[ (2 + 0 * N) * 2 ] = 1; Av[ (2 + 0 * N) * 2 + 1 ] = 0;
	Av[ (1 + 1 * N) * 2 ] = 4; Av[ (1 + 1 * N) * 2 + 1 ] = 0;
	Av[ (2 + 1 * N) * 2 ] = 1; Av[ (2 + 1 * N) * 2 + 1 ] = -1;
	Av[ (2 + 2 * N) * 2 ] = 6; Av[ (2 + 2 * N) * 2 + 1 ] = 1;

	var W = new Complex128Array( N * nb );
	var IPIV = new Int32Array( N );
	result = zlasyf( 'L', N, nb, A, 1, N, 0, IPIV, 1, 0, W, 1, N, 0 );
	assert.strictEqual( result.info, tc.info );
	assert.strictEqual( result.kb, tc.kb );
	expectedA = extractA( tc.A, N );
	assertArrayClose( Array.from( Av ), expectedA, 1e-10, 'A' );
	expected = fortranIPIVtoJS( tc.ipiv );
	for ( i = 0; i < N; i++ ) {
		assert.strictEqual( IPIV[ i ], expected[ i ], 'ipiv[' + i + ']' );
	}
});
