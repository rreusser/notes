

'use strict';

// MODULES //

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dggsvd3 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dggsvd3.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, offset, stride, expected, n, tol, msg ) {
	var i;
	for ( i = 0; i < n; i++ ) {
		assertClose( actual[ offset + ( i * stride ) ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dggsvd3: basic 3x3 A, 2x3 B', function t() {
	var tc = findCase( 'basic_3x3_2x3' );
	var M = 3; var N = 3; var P = 2;
	var A = new Float64Array( M * N );
	A[ 0 + 0*M ] = 1.0; A[ 0 + 1*M ] = 2.0; A[ 0 + 2*M ] = 3.0;
	A[ 1 + 0*M ] = 4.0; A[ 1 + 1*M ] = 5.0; A[ 1 + 2*M ] = 6.0;
	A[ 2 + 0*M ] = 7.0; A[ 2 + 1*M ] = 8.0; A[ 2 + 2*M ] = 10.0;
	var B = new Float64Array( P * N );
	B[ 0 + 0*P ] = 1.0; B[ 0 + 1*P ] = 0.0; B[ 0 + 2*P ] = 1.0;
	B[ 1 + 0*P ] = 0.0; B[ 1 + 1*P ] = 1.0; B[ 1 + 2*P ] = 1.0;
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( M * M );
	var V = new Float64Array( P * P );
	var Q = new Float64Array( N * N );
	var WORK = new Float64Array( 200 );
	var IWORK = new Int32Array( N );
	var K = new Int32Array( 1 );
	var L = new Int32Array( 1 );
	var info = dggsvd3( 'compute-U', 'compute-V', 'compute-Q', M, N, P, K, L,
		A, 1, M, 0, B, 1, P, 0,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, M, 0, V, 1, P, 0, Q, 1, N, 0,
		WORK, 1, 0, 200, IWORK, 1, 0 );
	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.k );
	assert.equal( L[ 0 ], tc.l );
	assertArrayClose( ALPHA, 0, 1, tc.alpha, N, 1e-2, 'alpha' );
	assertArrayClose( BETA, 0, 1, tc.beta, N, 1e-2, 'beta' );
});

test( 'dggsvd3: 2x3 A, 3x3 B', function t() {
	var tc = findCase( '2x3_3x3' );
	var M = 2; var N = 3; var P = 3;
	var A = new Float64Array( M * N );
	A[ 0 + 0*M ] = 2.0; A[ 0 + 1*M ] = 1.0; A[ 0 + 2*M ] = 0.0;
	A[ 1 + 0*M ] = 0.0; A[ 1 + 1*M ] = 3.0; A[ 1 + 2*M ] = 1.0;
	var B = new Float64Array( P * N );
	B[ 0 + 0*P ] = 1.0; B[ 0 + 1*P ] = 2.0; B[ 0 + 2*P ] = 3.0;
	B[ 1 + 0*P ] = 4.0; B[ 1 + 1*P ] = 5.0; B[ 1 + 2*P ] = 6.0;
	B[ 2 + 0*P ] = 7.0; B[ 2 + 1*P ] = 8.0; B[ 2 + 2*P ] = 10.0;
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( M * M );
	var V = new Float64Array( P * P );
	var Q = new Float64Array( N * N );
	var WORK = new Float64Array( 200 );
	var IWORK = new Int32Array( N );
	var K = new Int32Array( 1 );
	var L = new Int32Array( 1 );
	var info = dggsvd3( 'compute-U', 'compute-V', 'compute-Q', M, N, P, K, L,
		A, 1, M, 0, B, 1, P, 0,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, M, 0, V, 1, P, 0, Q, 1, N, 0,
		WORK, 1, 0, 200, IWORK, 1, 0 );
	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.k );
	assert.equal( L[ 0 ], tc.l );
	assertArrayClose( ALPHA, 0, 1, tc.alpha, N, 1e-2, 'alpha' );
	assertArrayClose( BETA, 0, 1, tc.beta, N, 1e-2, 'beta' );
});

test( 'dggsvd3: no U/V/Q', function t() {
	var tc = findCase( 'no_uvq' );
	var M = 3; var N = 3; var P = 2;
	var A = new Float64Array( M * N );
	A[ 0 + 0*M ] = 1.0; A[ 0 + 1*M ] = 2.0; A[ 0 + 2*M ] = 3.0;
	A[ 1 + 0*M ] = 4.0; A[ 1 + 1*M ] = 5.0; A[ 1 + 2*M ] = 6.0;
	A[ 2 + 0*M ] = 7.0; A[ 2 + 1*M ] = 8.0; A[ 2 + 2*M ] = 10.0;
	var B = new Float64Array( P * N );
	B[ 0 + 0*P ] = 1.0; B[ 0 + 1*P ] = 0.0; B[ 0 + 2*P ] = 1.0;
	B[ 1 + 0*P ] = 0.0; B[ 1 + 1*P ] = 1.0; B[ 1 + 2*P ] = 1.0;
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( 1 );
	var V = new Float64Array( 1 );
	var Q = new Float64Array( 1 );
	var WORK = new Float64Array( 200 );
	var IWORK = new Int32Array( N );
	var K = new Int32Array( 1 );
	var L = new Int32Array( 1 );
	var info = dggsvd3( 'none', 'none', 'none', M, N, P, K, L,
		A, 1, M, 0, B, 1, P, 0,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, 1, 0, V, 1, 1, 0, Q, 1, 1, 0,
		WORK, 1, 0, 200, IWORK, 1, 0 );
	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.k );
	assert.equal( L[ 0 ], tc.l );
	assertArrayClose( ALPHA, 0, 1, tc.alpha, N, 1e-2, 'alpha' );
	assertArrayClose( BETA, 0, 1, tc.beta, N, 1e-2, 'beta' );
});

test( 'dggsvd3: diagonal 4x4 A, 3x4 B', function t() {
	var tc = findCase( 'diag_4x4' );
	var M = 4; var N = 4; var P = 3;
	var A = new Float64Array( M * N );
	A[ 0 + 0*M ] = 1.0;
	A[ 1 + 1*M ] = 2.0;
	A[ 2 + 2*M ] = 3.0;
	A[ 3 + 3*M ] = 4.0;
	var B = new Float64Array( P * N );
	B[ 0 + 0*P ] = 1.0; B[ 0 + 1*P ] = 1.0;
	B[ 1 + 1*P ] = 1.0; B[ 1 + 2*P ] = 1.0;
	B[ 2 + 2*P ] = 1.0; B[ 2 + 3*P ] = 1.0;
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( M * M );
	var V = new Float64Array( P * P );
	var Q = new Float64Array( N * N );
	var WORK = new Float64Array( 200 );
	var IWORK = new Int32Array( N );
	var K = new Int32Array( 1 );
	var L = new Int32Array( 1 );
	var info = dggsvd3( 'compute-U', 'compute-V', 'compute-Q', M, N, P, K, L,
		A, 1, M, 0, B, 1, P, 0,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, M, 0, V, 1, P, 0, Q, 1, N, 0,
		WORK, 1, 0, 200, IWORK, 1, 0 );
	assert.equal( info, tc.info );
	assert.equal( K[ 0 ], tc.k );
	assert.equal( L[ 0 ], tc.l );
	// Verify ALPHA(1:K) = 1, BETA(1:K) = 0
	var kval = K[ 0 ];
	var lval = L[ 0 ];
	var i;
	for ( i = 0; i < kval; i++ ) {
		assertClose( ALPHA[ i ], 1.0, 1e-12, 'alpha[' + i + '] = 1' );
		assertClose( BETA[ i ], 0.0, 1e-12, 'beta[' + i + '] = 0' );
	}
	// Verify alpha^2 + beta^2 = 1 for K+1..K+L
	for ( i = kval; i < kval + lval; i++ ) {
		var sumsq = ( ALPHA[ i ] * ALPHA[ i ] ) + ( BETA[ i ] * BETA[ i ] );
		assertClose( sumsq, 1.0, 1e-10, 'alpha^2+beta^2=1 at ' + i );
	}
});

test( 'dggsvd3: workspace query', function t() {
	var tc = findCase( 'workspace_query' );
	var M = 3; var N = 3; var P = 2;
	var A = new Float64Array( M * N );
	var B = new Float64Array( P * N );
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( M * M );
	var V = new Float64Array( P * P );
	var Q = new Float64Array( N * N );
	var WORK = new Float64Array( 200 );
	var IWORK = new Int32Array( N );
	var K = new Int32Array( 1 );
	var L = new Int32Array( 1 );
	var info = dggsvd3( 'compute-U', 'compute-V', 'compute-Q', M, N, P, K, L,
		A, 1, M, 0, B, 1, P, 0,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, M, 0, V, 1, P, 0, Q, 1, N, 0,
		WORK, 1, 0, -1, IWORK, 1, 0 );
	assert.equal( info, tc.info );
	assert.ok( WORK[ 0 ] >= 1, 'optimal lwork >= 1' );
});
