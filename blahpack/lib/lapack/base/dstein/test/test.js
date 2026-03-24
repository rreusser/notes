'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dstein = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dstein.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Extract column j (0-based) from column-major flat array with N rows.
*/
function getColumn( flat, N, j ) {
	var col = new Float64Array( N );
	var i;
	for ( i = 0; i < N; i++ ) {
		col[ i ] = flat[ j * N + i ];
	}
	return col;
}

/**
* Compute dot product of two arrays.
*/
function dot( a, b ) {
	var s = 0.0;
	var i;
	for ( i = 0; i < a.length; i++ ) {
		s += a[ i ] * b[ i ];
	}
	return s;
}

/**
* Check orthogonality of eigenvectors: Z^T * Z should be identity.
*/
function checkOrthogonality( Zflat, N, M, tol ) {
	var ci;
	var cj;
	var v;
	var expected;
	var i;
	var j;
	for ( i = 0; i < M; i++ ) {
		ci = getColumn( Zflat, N, i );
		for ( j = i; j < M; j++ ) {
			cj = getColumn( Zflat, N, j );
			v = dot( ci, cj );
			expected = ( i === j ) ? 1.0 : 0.0;
			assertClose( v, expected, tol, 'Z(:,' + i + ')^T * Z(:,' + j + ')' );
		}
	}
}


// TESTS //

test( 'dstein: basic 5x5, all eigenvectors', function t() {
	var tc = findCase( 'basic_5x5_all' );
	var N = 5;
	var M = 5;
	var d = new Float64Array( [ 2.0, 2.0, 2.0, 2.0, 2.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var w = new Float64Array( tc.w );
	var IBLOCK = new Int32Array( tc.iblock );
	var ISPLIT = new Int32Array( tc.isplit );
	var Z = new Float64Array( N * M );
	var WORK = new Float64Array( 5 * N );
	var IWORK = new Int32Array( N );
	var IFAIL = new Int32Array( M );

	var info = dstein( N, d, 1, 0, e, 1, 0, M, w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0, Z, 1, N, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );

	assert.equal( info, 0, 'info' );
	assert.deepEqual( Array.from( IFAIL ), [ 0, 0, 0, 0, 0 ], 'ifail' );

	// Check orthogonality
	checkOrthogonality( Z, N, M, 1e-12 );

	// Compare against fixture (eigenvectors can differ by sign)
	var i;
	var j;
	for ( j = 0; j < M; j++ ) {
		var colActual = getColumn( Z, N, j );
		var colExpected = getColumn( tc.Z, N, j );
		// Determine sign: compare first significant element
		var sign = 1.0;
		for ( i = 0; i < N; i++ ) {
			if ( Math.abs( colExpected[ i ] ) > 1e-10 ) {
				sign = ( colActual[ i ] * colExpected[ i ] > 0 ) ? 1.0 : -1.0;
				break;
			}
		}
		for ( i = 0; i < N; i++ ) {
			assertClose( colActual[ i ] * sign, colExpected[ i ], 1e-12, 'Z[' + i + ',' + j + ']' );
		}
	}
});

test( 'dstein: partial 2 of 5 eigenvectors', function t() {
	var tc = findCase( 'partial_2of5' );
	var N = 5;
	var M = 2;
	var d = new Float64Array( [ 2.0, 2.0, 2.0, 2.0, 2.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var w = new Float64Array( tc.w );
	var IBLOCK = new Int32Array( [ 1, 1 ] );
	var ISPLIT = new Int32Array( [ 5 ] );
	var Z = new Float64Array( N * M );
	var WORK = new Float64Array( 5 * N );
	var IWORK = new Int32Array( N );
	var IFAIL = new Int32Array( M );

	var info = dstein( N, d, 1, 0, e, 1, 0, M, w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0, Z, 1, N, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );

	assert.equal( info, 0, 'info' );
	assert.deepEqual( Array.from( IFAIL ), [ 0, 0 ], 'ifail' );
	checkOrthogonality( Z, N, M, 1e-12 );
});

test( 'dstein: N=1', function t() {
	var N = 1;
	var M = 1;
	var d = new Float64Array( [ 3.0 ] );
	var e = new Float64Array( 0 );
	var w = new Float64Array( [ 3.0 ] );
	var IBLOCK = new Int32Array( [ 1 ] );
	var ISPLIT = new Int32Array( [ 1 ] );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 5 );
	var IWORK = new Int32Array( 1 );
	var IFAIL = new Int32Array( 1 );

	var info = dstein( N, d, 1, 0, e, 1, 0, M, w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0, Z, 1, 1, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );

	assert.equal( info, 0, 'info' );
	assertClose( Z[ 0 ], 1.0, 1e-14, 'Z[0]' );
});

test( 'dstein: N=0', function t() {
	var d = new Float64Array( 0 );
	var e = new Float64Array( 0 );
	var w = new Float64Array( 0 );
	var IBLOCK = new Int32Array( 0 );
	var ISPLIT = new Int32Array( 0 );
	var Z = new Float64Array( 0 );
	var WORK = new Float64Array( 0 );
	var IWORK = new Int32Array( 0 );
	var IFAIL = new Int32Array( 0 );

	var info = dstein( 0, d, 1, 0, e, 1, 0, 0, w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0, Z, 1, 0, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );

	assert.equal( info, 0, 'info' );
});

test( 'dstein: two blocks', function t() {
	var tc = findCase( 'two_blocks' );
	var N = 5;
	var M = 5;
	var d = new Float64Array( [ 4.0, 4.0, 4.0, 3.0, 3.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 0.0, 0.5 ] );
	var w = new Float64Array( tc.w );
	var IBLOCK = new Int32Array( tc.iblock );
	var ISPLIT = new Int32Array( tc.isplit );
	var Z = new Float64Array( N * M );
	var WORK = new Float64Array( 5 * N );
	var IWORK = new Int32Array( N );
	var IFAIL = new Int32Array( M );

	var info = dstein( N, d, 1, 0, e, 1, 0, M, w, 1, 0, IBLOCK, 1, 0, ISPLIT, 1, 0, Z, 1, N, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );

	assert.equal( info, 0, 'info' );
	assert.deepEqual( Array.from( IFAIL ), [ 0, 0, 0, 0, 0 ], 'ifail' );

	// Check orthogonality within each block
	// Block 1: columns 0-2 (rows 0-2)
	// Block 2: columns 3-4 (rows 3-4)
	checkOrthogonality( Z, N, 3, 1e-12 ); // first 3 columns

	// Verify block structure: rows 3-4 of first 3 columns should be zero
	var i;
	var j;
	for ( j = 0; j < 3; j++ ) {
		for ( i = 3; i < 5; i++ ) {
			assertClose( Z[ j * N + i ], 0.0, 1e-14, 'Z[' + i + ',' + j + '] should be zero' );
		}
	}
	// Rows 0-2 of columns 3-4 should be zero
	for ( j = 3; j < 5; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			assertClose( Z[ j * N + i ], 0.0, 1e-14, 'Z[' + i + ',' + j + '] should be zero' );
		}
	}
});
