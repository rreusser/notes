'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlauum = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlauum.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Extracts a column-major flat array from a Float64Array matrix.
*
* @private
* @param {Float64Array} A - matrix
* @param {integer} N - order
* @param {integer} sa1 - stride of first dimension
* @param {integer} sa2 - stride of second dimension
* @param {integer} offset - starting offset
* @returns {Array} column-major flat array
*/
function extractColMajor( A, N, sa1, sa2, offset ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			out.push( A[ offset + i * sa1 + j * sa2 ] );
		}
	}
	return out;
}


// TESTS //

test( 'dlauum: upper 4x4 (U*U^T)', function t() {
	var tc = findCase( 'upper_4' );
	var N = 4;
	var A = new Float64Array( N * N );
	var info;

	// U = [2 1 3 2; 0 4 1 3; 0 0 5 1; 0 0 0 6] (column-major)
	A[ 0 ] = 2.0; A[ 4 ] = 1.0; A[ 8 ]  = 3.0; A[ 12 ] = 2.0;
	               A[ 5 ] = 4.0; A[ 9 ]  = 1.0; A[ 13 ] = 3.0;
	                              A[ 10 ] = 5.0; A[ 14 ] = 1.0;
	                                              A[ 15 ] = 6.0;

	info = dlauum( 'U', N, A, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( extractColMajor( A, N, 1, N, 0 ), tc.a, 1e-14, 'a' );
});

test( 'dlauum: lower 4x4 (L^T*L)', function t() {
	var tc = findCase( 'lower_4' );
	var N = 4;
	var A = new Float64Array( N * N );
	var info;

	// L = [2 0 0 0; 1 4 0 0; 3 1 5 0; 2 3 1 6] (column-major)
	A[ 0 ] = 2.0; A[ 1 ] = 1.0; A[ 2 ]  = 3.0; A[ 3 ]  = 2.0;
	               A[ 5 ] = 4.0; A[ 6 ]  = 1.0; A[ 7 ]  = 3.0;
	                              A[ 10 ] = 5.0; A[ 11 ] = 1.0;
	                                              A[ 15 ] = 6.0;

	info = dlauum( 'L', N, A, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( extractColMajor( A, N, 1, N, 0 ), tc.a, 1e-14, 'a' );
});

test( 'dlauum: N=1 upper', function t() {
	var tc = findCase( 'n1_upper' );
	var A = new Float64Array( [ 3.0 ] );
	var info;

	info = dlauum( 'U', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlauum: N=1 lower', function t() {
	var tc = findCase( 'n1_lower' );
	var A = new Float64Array( [ 5.0 ] );
	var info;

	info = dlauum( 'L', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlauum: N=0 quick return', function t() {
	var tc = findCase( 'n0' );
	var A = new Float64Array( 1 );
	var info;

	info = dlauum( 'U', 0, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dlauum: upper 35x35 (blocked path)', function t() {
	var tc = findCase( 'upper_35' );
	var N = 35;
	var A = new Float64Array( N * N );
	var info;
	var i;
	var j;

	// Build diagonally dominant upper triangular matrix (matching Fortran)
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i <= j; i++ ) {
			if ( i === j ) {
				A[ i + j * N ] = N + 1;
			} else {
				A[ i + j * N ] = 1.0 / ( j - i + 1 );
			}
		}
	}

	info = dlauum( 'U', N, A, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( extractColMajor( A, N, 1, N, 0 ), tc.a, 1e-12, 'a' );
});

test( 'dlauum: lower 35x35 (blocked path)', function t() {
	var tc = findCase( 'lower_35' );
	var N = 35;
	var A = new Float64Array( N * N );
	var info;
	var i;
	var j;

	// Build diagonally dominant lower triangular matrix (matching Fortran)
	for ( j = 0; j < N; j++ ) {
		for ( i = j; i < N; i++ ) {
			if ( i === j ) {
				A[ i + j * N ] = N + 1;
			} else {
				A[ i + j * N ] = 1.0 / ( i - j + 1 );
			}
		}
	}

	info = dlauum( 'L', N, A, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( extractColMajor( A, N, 1, N, 0 ), tc.a, 1e-12, 'a' );
});
