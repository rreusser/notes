

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dggsvp3 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dggsvp3.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function extractMatrix( arr, offset, strideA1, strideA2, m, n ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i < m; i++ ) {
			out.push( arr[ offset + (i * strideA1) + (j * strideA2) ] );
		}
	}
	return out;
}

/**
* Verify that a matrix is orthogonal: Q^T * Q = I.
*/
function assertOrthogonal( Q, n, stride1, stride2, offset, tol, msg ) {
	var dot;
	var i;
	var j;
	var k;
	for ( i = 0; i < n; i++ ) {
		for ( j = i; j < n; j++ ) {
			dot = 0.0;
			for ( k = 0; k < n; k++ ) {
				dot += Q[ offset + (k * stride1) + (i * stride2) ] * Q[ offset + (k * stride1) + (j * stride2) ];
			}
			if ( i === j ) {
				assertClose( dot, 1.0, tol, msg + ': col ' + i + ' norm' );
			} else {
				assert.ok( Math.abs( dot ) < tol, msg + ': cols ' + i + ',' + j + ' dot=' + dot );
			}
		}
	}
}


// TESTS //

test( 'dggsvp3: basic_4x3_3x3_UVQ', function t() {
	var tc = findCase( 'basic_4x3_3x3_UVQ' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var B = new Float64Array( [ 10, 1, 1, 1, 10, 1, 1, 1, 10 ] );
	var U = new Float64Array( 16 );
	var V = new Float64Array( 9 );
	var Q = new Float64Array( 9 );
	var IWORK = new Int32Array( 3 );
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 5000 );
	var K = [ 0 ];
	var l = [ 0 ];
	var info = dggsvp3( 'compute-U', 'compute-V', 'compute-Q', 4, 3, 3, A, 1, 4, 0, B, 1, 3, 0, 1e-8, 1e-8, K, l, U, 1, 4, 0, V, 1, 3, 0, Q, 1, 3, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, 5000 );

	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.K );
	assert.equal( l[ 0 ], tc.L );
	assertArrayClose( extractMatrix( A, 0, 1, 4, 4, 3 ), tc.A, 1e-12, 'A' );
	assertArrayClose( extractMatrix( B, 0, 1, 3, 3, 3 ), tc.B, 1e-12, 'B' );
	// U may differ in null-space columns for rank-deficient input; verify orthogonality
	assertOrthogonal( U, 4, 1, 4, 0, 1e-12, 'U orthogonal' );
	assertArrayClose( extractMatrix( V, 0, 1, 3, 3, 3 ), tc.V, 1e-12, 'V' );
	assertArrayClose( extractMatrix( Q, 0, 1, 3, 3, 3 ), tc.Q, 1e-12, 'Q' );
});

test( 'dggsvp3: basic_4x3_3x3_NNN', function t() {
	var tc = findCase( 'basic_4x3_3x3_NNN' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var B = new Float64Array( [ 10, 1, 1, 1, 10, 1, 1, 1, 10 ] );
	var U = new Float64Array( 1 );
	var V = new Float64Array( 1 );
	var Q = new Float64Array( 1 );
	var IWORK = new Int32Array( 3 );
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 5000 );
	var K = [ 0 ];
	var l = [ 0 ];
	var info = dggsvp3( 'none', 'none', 'none', 4, 3, 3, A, 1, 4, 0, B, 1, 3, 0, 1e-8, 1e-8, K, l, U, 1, 1, 0, V, 1, 1, 0, Q, 1, 1, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, 5000 );

	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.K );
	assert.equal( l[ 0 ], tc.L );
	assertArrayClose( extractMatrix( A, 0, 1, 4, 4, 3 ), tc.A, 1e-12, 'A' );
	assertArrayClose( extractMatrix( B, 0, 1, 3, 3, 3 ), tc.B, 1e-12, 'B' );
});

test( 'dggsvp3: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( [ 5, 1, 1, 5 ] );
	var U = new Float64Array( 1 );
	var V = new Float64Array( 4 );
	var Q = new Float64Array( 4 );
	var IWORK = new Int32Array( 2 );
	var TAU = new Float64Array( 2 );
	var WORK = new Float64Array( 5000 );
	var K = [ 0 ];
	var l = [ 0 ];
	var info = dggsvp3( 'compute-U', 'compute-V', 'compute-Q', 0, 2, 2, A, 1, 1, 0, B, 1, 2, 0, 1e-8, 1e-8, K, l, U, 1, 1, 0, V, 1, 2, 0, Q, 1, 2, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, 5000 );

	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.K );
	assert.equal( l[ 0 ], tc.L );
});

test( 'dggsvp3: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var U = new Float64Array( 9 );
	var V = new Float64Array( 4 );
	var Q = new Float64Array( 1 );
	var IWORK = new Int32Array( 1 );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 5000 );
	var K = [ 0 ];
	var l = [ 0 ];
	var info = dggsvp3( 'compute-U', 'compute-V', 'compute-Q', 3, 2, 0, A, 1, 3, 0, B, 1, 2, 0, 1e-8, 1e-8, K, l, U, 1, 3, 0, V, 1, 2, 0, Q, 1, 1, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, 5000 );

	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.K );
	assert.equal( l[ 0 ], tc.L );
});

test( 'dggsvp3: p_zero', function t() {
	var tc = findCase( 'p_zero' );
	var A = new Float64Array( [ 5, 1, 1, 5 ] );
	var B = new Float64Array( 1 );
	var U = new Float64Array( 4 );
	var V = new Float64Array( 1 );
	var Q = new Float64Array( 4 );
	var IWORK = new Int32Array( 2 );
	var TAU = new Float64Array( 2 );
	var WORK = new Float64Array( 5000 );
	var K = [ 0 ];
	var l = [ 0 ];
	var info = dggsvp3( 'compute-U', 'compute-V', 'compute-Q', 2, 0, 2, A, 1, 2, 0, B, 1, 1, 0, 1e-8, 1e-8, K, l, U, 1, 2, 0, V, 1, 1, 0, Q, 1, 2, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, 5000 );

	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.K );
	assert.equal( l[ 0 ], tc.L );
});

test( 'dggsvp3: rank_deficient_B', function t() {
	var tc = findCase( 'rank_deficient_B' );
	var A = new Float64Array( [ 2, 1, 0, 1, 3, 1, 0, 1, 4 ] );
	var B = new Float64Array( [ 5, 1, 0, 1, 5, 0, 1, 1, 0 ] );
	var U = new Float64Array( 9 );
	var V = new Float64Array( 9 );
	var Q = new Float64Array( 9 );
	var IWORK = new Int32Array( 3 );
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 5000 );
	var K = [ 0 ];
	var l = [ 0 ];
	var info = dggsvp3( 'compute-U', 'compute-V', 'compute-Q', 3, 3, 3, A, 1, 3, 0, B, 1, 3, 0, 1e-8, 1e-8, K, l, U, 1, 3, 0, V, 1, 3, 0, Q, 1, 3, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, 5000 );

	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.K );
	assert.equal( l[ 0 ], tc.L );
	assertArrayClose( extractMatrix( A, 0, 1, 3, 3, 3 ), tc.A, 1e-12, 'A' );
	assertArrayClose( extractMatrix( B, 0, 1, 3, 3, 3 ), tc.B, 1e-12, 'B' );
	assertArrayClose( extractMatrix( U, 0, 1, 3, 3, 3 ), tc.U, 1e-12, 'U' );
	assertArrayClose( extractMatrix( V, 0, 1, 3, 3, 3 ), tc.V, 1e-12, 'V' );
	assertArrayClose( extractMatrix( Q, 0, 1, 3, 3, 3 ), tc.Q, 1e-12, 'Q' );
});

test( 'dggsvp3: wide_2x5_UVQ', function t() {
	var tc = findCase( 'wide_2x5_UVQ' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
	var B = new Float64Array( [ 10, 1, 1, 10, 2, 2, 3, 3, 1, 1 ] );
	var U = new Float64Array( 4 );
	var V = new Float64Array( 4 );
	var Q = new Float64Array( 25 );
	var IWORK = new Int32Array( 5 );
	var TAU = new Float64Array( 5 );
	var WORK = new Float64Array( 5000 );
	var K = [ 0 ];
	var l = [ 0 ];
	var info = dggsvp3( 'compute-U', 'compute-V', 'compute-Q', 2, 2, 5, A, 1, 2, 0, B, 1, 2, 0, 1e-8, 1e-8, K, l, U, 1, 2, 0, V, 1, 2, 0, Q, 1, 5, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, 5000 );

	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.K );
	assert.equal( l[ 0 ], tc.L );
	assertArrayClose( extractMatrix( A, 0, 1, 2, 2, 5 ), tc.A, 1e-12, 'A' );
	assertArrayClose( extractMatrix( B, 0, 1, 2, 2, 5 ), tc.B, 1e-12, 'B' );
	assertArrayClose( extractMatrix( U, 0, 1, 2, 2, 2 ), tc.U, 1e-12, 'U' );
	assertArrayClose( extractMatrix( V, 0, 1, 2, 2, 2 ), tc.V, 1e-12, 'V' );
	assertArrayClose( extractMatrix( Q, 0, 1, 5, 5, 5 ), tc.Q, 1e-12, 'Q' );
});

test( 'dggsvp3: diagonal_3x3', function t() {
	var tc = findCase( 'diagonal_3x3' );
	var A = new Float64Array( [ 10, 0, 0, 0, 5, 0, 0, 0, 1 ] );
	var B = new Float64Array( [ 8, 0, 0, 0, 4, 0, 0, 0, 2 ] );
	var U = new Float64Array( 9 );
	var V = new Float64Array( 9 );
	var Q = new Float64Array( 9 );
	var IWORK = new Int32Array( 3 );
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 5000 );
	var K = [ 0 ];
	var l = [ 0 ];
	var info = dggsvp3( 'compute-U', 'compute-V', 'compute-Q', 3, 3, 3, A, 1, 3, 0, B, 1, 3, 0, 1e-8, 1e-8, K, l, U, 1, 3, 0, V, 1, 3, 0, Q, 1, 3, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, 5000 );

	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.K );
	assert.equal( l[ 0 ], tc.L );
	assertArrayClose( extractMatrix( A, 0, 1, 3, 3, 3 ), tc.A, 1e-12, 'A' );
	assertArrayClose( extractMatrix( B, 0, 1, 3, 3, 3 ), tc.B, 1e-12, 'B' );
	assertArrayClose( extractMatrix( U, 0, 1, 3, 3, 3 ), tc.U, 1e-12, 'U' );
	assertArrayClose( extractMatrix( V, 0, 1, 3, 3, 3 ), tc.V, 1e-12, 'V' );
	assertArrayClose( extractMatrix( Q, 0, 1, 3, 3, 3 ), tc.Q, 1e-12, 'Q' );
});

test( 'dggsvp3: tall_B_3x5x3', function t() {
	var tc = findCase( 'tall_B_3x5x3' );
	var A = new Float64Array( [ 2, 1, 0.5, 0.5, 3, 1, 1, 0.5, 4 ] );
	var B = new Float64Array( [ 10, 1, 1, 1, 1, 1, 10, 1, 1, 1, 1, 1, 10, 1, 1 ] );
	var U = new Float64Array( 9 );
	var V = new Float64Array( 25 );
	var Q = new Float64Array( 9 );
	var IWORK = new Int32Array( 3 );
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 5000 );
	var K = [ 0 ];
	var l = [ 0 ];
	var info = dggsvp3( 'compute-U', 'compute-V', 'compute-Q', 3, 5, 3, A, 1, 3, 0, B, 1, 5, 0, 1e-8, 1e-8, K, l, U, 1, 3, 0, V, 1, 5, 0, Q, 1, 3, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, 5000 );

	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.K );
	assert.equal( l[ 0 ], tc.L );
	assertArrayClose( extractMatrix( A, 0, 1, 3, 3, 3 ), tc.A, 1e-12, 'A' );
	assertArrayClose( extractMatrix( B, 0, 1, 5, 5, 3 ), tc.B, 1e-12, 'B' );
	assertArrayClose( extractMatrix( U, 0, 1, 3, 3, 3 ), tc.U, 1e-12, 'U' );
	assertArrayClose( extractMatrix( V, 0, 1, 5, 5, 5 ), tc.V, 1e-12, 'V' );
	assertArrayClose( extractMatrix( Q, 0, 1, 3, 3, 3 ), tc.Q, 1e-12, 'Q' );
});
