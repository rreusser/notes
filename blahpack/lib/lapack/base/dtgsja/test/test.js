

'use strict';

// MODULES //

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtgsja = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtgsja.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dtgsja: basic 3x3 with K=1, L=2', function t() {
	var tc = findCase( 'basic_3x3' );
	var M = 3; var P = 2; var N = 3; var K = 1; var L = 2;
	// Column-major M-by-N matrix, packed densely
	var A = new Float64Array( M * N ); // 3x3
	A[ 0 + 0*M ] = 0.0; A[ 0 + 1*M ] = 3.0; A[ 0 + 2*M ] = 1.0;
	A[ 1 + 0*M ] = 0.0; A[ 1 + 1*M ] = 0.0; A[ 1 + 2*M ] = 4.0;
	A[ 2 + 0*M ] = 0.0; A[ 2 + 1*M ] = 0.0; A[ 2 + 2*M ] = 0.0;
	var B = new Float64Array( P * N ); // 2x3
	B[ 0 + 0*P ] = 0.0; B[ 0 + 1*P ] = 2.0; B[ 0 + 2*P ] = 0.5;
	B[ 1 + 0*P ] = 0.0; B[ 1 + 1*P ] = 0.0; B[ 1 + 2*P ] = 3.0;
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( M * M );
	var V = new Float64Array( P * P );
	var Q = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * N );
	var ncycle = new Int32Array( 1 );
	var info = dtgsja( 'initialize', 'initialize', 'initialize', M, P, N, K, L,
		A, 1, M, 0, B, 1, P, 0, 1e-14, 1e-14,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, M, 0, V, 1, P, 0, Q, 1, N, 0,
		WORK, 1, 0, ncycle );
	assert.equal( info, tc.info );
	assert.equal( ncycle[ 0 ], tc.ncycle );
	assertArrayClose( ALPHA, 0, 1, tc.alpha, N, 1e-12, 'alpha' );
	assertArrayClose( BETA, 0, 1, tc.beta, N, 1e-12, 'beta' );
});

test( 'dtgsja: K=0, L=2', function t() {
	var tc = findCase( 'k0_l2' );
	var M = 2; var P = 2; var N = 2; var K = 0; var L = 2;
	var A = new Float64Array( M * N );
	A[ 0 + 0*M ] = 5.0; A[ 0 + 1*M ] = 2.0;
	A[ 1 + 0*M ] = 0.0; A[ 1 + 1*M ] = 3.0;
	var B = new Float64Array( P * N );
	B[ 0 + 0*P ] = 4.0; B[ 0 + 1*P ] = 1.0;
	B[ 1 + 0*P ] = 0.0; B[ 1 + 1*P ] = 2.0;
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( M * M );
	var V = new Float64Array( P * P );
	var Q = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * N );
	var ncycle = new Int32Array( 1 );
	var info = dtgsja( 'initialize', 'initialize', 'initialize', M, P, N, K, L,
		A, 1, M, 0, B, 1, P, 0, 1e-14, 1e-14,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, M, 0, V, 1, P, 0, Q, 1, N, 0,
		WORK, 1, 0, ncycle );
	assert.equal( info, tc.info );
	assertArrayClose( ALPHA, 0, 1, tc.alpha, N, 1e-12, 'alpha' );
	assertArrayClose( BETA, 0, 1, tc.beta, N, 1e-12, 'beta' );
});

test( 'dtgsja: K=2, L=1', function t() {
	var tc = findCase( 'k2_l1' );
	var M = 4; var P = 2; var N = 4; var K = 2; var L = 1;
	var A = new Float64Array( M * N );
	A[ 0 + 2*M ] = 1.0;
	A[ 1 + 3*M ] = 2.0;
	A[ 2 + 3*M ] = 5.0;
	var B = new Float64Array( P * N );
	B[ 0 + 3*P ] = 3.0;
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( M * M );
	var V = new Float64Array( P * P );
	var Q = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * N );
	var ncycle = new Int32Array( 1 );
	var info = dtgsja( 'initialize', 'initialize', 'initialize', M, P, N, K, L,
		A, 1, M, 0, B, 1, P, 0, 1e-14, 1e-14,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, M, 0, V, 1, P, 0, Q, 1, N, 0,
		WORK, 1, 0, ncycle );
	assert.equal( info, tc.info );
	assertArrayClose( ALPHA, 0, 1, tc.alpha, N, 1e-12, 'alpha' );
	assertArrayClose( BETA, 0, 1, tc.beta, N, 1e-12, 'beta' );
});

test( 'dtgsja: no U/V/Q', function t() {
	var tc = findCase( 'no_uvq' );
	var M = 2; var P = 2; var N = 2; var K = 0; var L = 2;
	var A = new Float64Array( M * N );
	A[ 0 + 0*M ] = 5.0; A[ 0 + 1*M ] = 2.0;
	A[ 1 + 0*M ] = 0.0; A[ 1 + 1*M ] = 3.0;
	var B = new Float64Array( P * N );
	B[ 0 + 0*P ] = 4.0; B[ 0 + 1*P ] = 1.0;
	B[ 1 + 0*P ] = 0.0; B[ 1 + 1*P ] = 2.0;
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( 1 );
	var V = new Float64Array( 1 );
	var Q = new Float64Array( 1 );
	var WORK = new Float64Array( 2 * N );
	var ncycle = new Int32Array( 1 );
	var info = dtgsja( 'none', 'none', 'none', M, P, N, K, L,
		A, 1, M, 0, B, 1, P, 0, 1e-14, 1e-14,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, 1, 0, V, 1, 1, 0, Q, 1, 1, 0,
		WORK, 1, 0, ncycle );
	assert.equal( info, tc.info );
	assertArrayClose( ALPHA, 0, 1, tc.alpha, N, 1e-12, 'alpha' );
	assertArrayClose( BETA, 0, 1, tc.beta, N, 1e-12, 'beta' );
});

test( 'dtgsja: M-K-L < 0 case', function t() {
	var tc = findCase( 'm_k_l_negative' );
	var M = 2; var P = 3; var N = 4; var K = 1; var L = 2;
	var A = new Float64Array( M * N );
	A[ 0 + 2*M ] = 2.0; A[ 0 + 3*M ] = 1.0;
	A[ 1 + 3*M ] = 4.0;
	var B = new Float64Array( P * N );
	B[ 0 + 2*P ] = 3.0; B[ 0 + 3*P ] = 0.5;
	B[ 1 + 3*P ] = 2.0;
	var ALPHA = new Float64Array( N );
	var BETA = new Float64Array( N );
	var U = new Float64Array( M * M );
	var V = new Float64Array( P * P );
	var Q = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * N );
	var ncycle = new Int32Array( 1 );
	var info = dtgsja( 'initialize', 'initialize', 'initialize', M, P, N, K, L,
		A, 1, M, 0, B, 1, P, 0, 1e-14, 1e-14,
		ALPHA, 1, 0, BETA, 1, 0,
		U, 1, M, 0, V, 1, P, 0, Q, 1, N, 0,
		WORK, 1, 0, ncycle );
	assert.equal( info, tc.info );
	assertArrayClose( ALPHA, 0, 1, tc.alpha, N, 1e-12, 'alpha' );
	assertArrayClose( BETA, 0, 1, tc.beta, N, 1e-12, 'beta' );
});
