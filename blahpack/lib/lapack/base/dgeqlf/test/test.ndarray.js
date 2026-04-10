/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var base = require( './../lib/base.js' );
var ndarrayFn = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgeqlf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'base is a function', function t() {
	assert.strictEqual( typeof base, 'function', 'is a function' );
});

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarrayFn, 'function', 'is a function' );
});

test( 'ndarray: 3x3 fixture via column-major layout (strideA1=1, strideA2=M)', function t() {
	var tc = findCase( '3x3' );
	var src = [ 2, 1, 3, 1, 4, 2, 3, 2, 5 ];
	var A = new Float64Array( src );
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 );
	var info = ndarrayFn( 3, 3, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );
	assert.equal( info, tc.INFO, 'info' );
	assertArrayClose( A, tc.A, 1e-13, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-13, 'TAU' );
});

test( 'ndarray: 3x3 fixture via row-major layout (strideA1=N, strideA2=1)', function t() {
	// Row-major layout isn't expressible via the flat base() API: strideA1 != 1.
	var tc = findCase( '3x3' );
	var cmSrc = [ 2, 1, 3, 1, 4, 2, 3, 2, 5 ];
	var M = 3;
	var N = 3;
	var A = new Float64Array( M * N );
	// Convert column-major source into row-major storage.
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ ( i * N ) + j ] = cmSrc[ i + ( j * M ) ];
		}
	}
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 );
	var info = ndarrayFn( M, N, A, N, 1, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );
	assert.equal( info, tc.INFO, 'info' );
	// Compare row-major A to transposed fixture (which is column-major).
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			assertClose( A[ ( i * N ) + j ], tc.A[ i + ( j * M ) ], 1e-13, 'A[' + i + ',' + j + ']' );
		}
	}
	assertArrayClose( TAU, tc.TAU, 1e-13, 'TAU' );
});

test( 'ndarray: honors offsetA with sentinel padding', function t() {
	var tc = findCase( '3x3' );
	var src = [ 2, 1, 3, 1, 4, 2, 3, 2, 5 ];
	var SENTINEL = -9.99e99;
	var offsetA = 4;
	var pad = 3;
	var buf = new Float64Array( offsetA + src.length + pad );
	var k;
	for ( k = 0; k < buf.length; k++ ) {
		buf[ k ] = SENTINEL;
	}
	for ( k = 0; k < src.length; k++ ) {
		buf[ offsetA + k ] = src[ k ];
	}
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 );
	var info = ndarrayFn( 3, 3, buf, 1, 3, offsetA, TAU, 1, 0, WORK, 1, 0, WORK.length );
	assert.equal( info, tc.INFO, 'info' );
	// Verify sentinels outside the logical window were not touched.
	for ( k = 0; k < offsetA; k++ ) {
		assert.strictEqual( buf[ k ], SENTINEL, 'pre-sentinel[' + k + ']' );
	}
	for ( k = 0; k < pad; k++ ) {
		assert.strictEqual( buf[ offsetA + src.length + k ], SENTINEL, 'post-sentinel[' + k + ']' );
	}
	// Verify numerical output.
	for ( k = 0; k < tc.A.length; k++ ) {
		assertClose( buf[ offsetA + k ], tc.A[ k ], 1e-13, 'A[' + k + ']' );
	}
	assertArrayClose( TAU, tc.TAU, 1e-13, 'TAU' );
});

test( 'ndarray: honors offsetTAU with sentinel padding', function t() {
	var tc = findCase( '4x3' );
	var src = [ 2, 1, 3, 1, 1, 4, 2, 3, 3, 2, 5, 1 ];
	var A = new Float64Array( src );
	var SENTINEL = 7.77e77;
	var offsetTAU = 2;
	var tauPad = 2;
	var TAU = new Float64Array( offsetTAU + 3 + tauPad );
	var k;
	for ( k = 0; k < TAU.length; k++ ) {
		TAU[ k ] = SENTINEL;
	}
	var WORK = new Float64Array( 16 );
	var info = ndarrayFn( 4, 3, A, 1, 4, 0, TAU, 1, offsetTAU, WORK, 1, 0, WORK.length );
	assert.equal( info, tc.INFO, 'info' );
	for ( k = 0; k < offsetTAU; k++ ) {
		assert.strictEqual( TAU[ k ], SENTINEL, 'pre-TAU-sentinel[' + k + ']' );
	}
	for ( k = 0; k < tauPad; k++ ) {
		assert.strictEqual( TAU[ offsetTAU + 3 + k ], SENTINEL, 'post-TAU-sentinel[' + k + ']' );
	}
	for ( k = 0; k < tc.TAU.length; k++ ) {
		assertClose( TAU[ offsetTAU + k ], tc.TAU[ k ], 1e-13, 'TAU[' + k + ']' );
	}
	assertArrayClose( A, tc.A, 1e-13, 'A' );
});

test( 'ndarray: LDA > M (column-major with extra padding rows)', function t() {
	var tc = findCase( '3x3' );
	var src = [ 2, 1, 3, 1, 4, 2, 3, 2, 5 ];
	var M = 3;
	var N = 3;
	var lda = 5; // strideA2 = 5, rows 3..4 are padding
	var SENTINEL = 1.23e45;
	var buf = new Float64Array( lda * N );
	var i;
	var j;
	for ( i = 0; i < buf.length; i++ ) {
		buf[ i ] = SENTINEL;
	}
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			buf[ i + ( j * lda ) ] = src[ i + ( j * M ) ];
		}
	}
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 );
	var info = ndarrayFn( M, N, buf, 1, lda, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );
	assert.equal( info, tc.INFO, 'info' );
	// Verify padding rows untouched.
	for ( j = 0; j < N; j++ ) {
		for ( i = M; i < lda; i++ ) {
			assert.strictEqual( buf[ i + ( j * lda ) ], SENTINEL, 'padding[' + i + ',' + j + ']' );
		}
	}
	// Verify logical rows match fixture.
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			assertClose( buf[ i + ( j * lda ) ], tc.A[ i + ( j * M ) ], 1e-13, 'A[' + i + ',' + j + ']' );
		}
	}
	assertArrayClose( TAU, tc.TAU, 1e-13, 'TAU' );
});
