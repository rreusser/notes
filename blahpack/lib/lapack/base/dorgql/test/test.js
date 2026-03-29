'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dorgql = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorgql.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Run dorgql on a test case. Reads input from the *_input fixture case,
* calls dorgql, and returns the result Q as a flat column-major array.
*
* @param {string} inputName - name of the _input fixture case
* @returns {Object} { info, Q (Float64Array, M*N flat col-major) }
*/
function runCase( inputName ) {
	var tc = findCase( inputName );
	var M = tc.M;
	var N = tc.N;
	var K = tc.K;
	var A = new Float64Array( tc.A );
	var TAU = new Float64Array( tc.TAU );
	var WORK = new Float64Array( Math.max( 1, N ) );
	var info;

	// Column-major: strideA1=1, strideA2=M
	info = dorgql(M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	return { info: info, Q: A };
}

/**
* Compute Q^T * Q for an M-by-N column-major matrix and return as flat N*N array.
*/
function computeQtQ( Q, M, N ) {
	var result = new Float64Array( N * N );
	var i;
	var j;
	var k;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			for ( k = 0; k < M; k++ ) {
				result[ i + j * N ] += Q[ k + i * M ] * Q[ k + j * M ];
			}
		}
	}
	return result;
}


// TESTS //

test( 'dorgql: 4x3, K=3 (M > N, full K)', function t() {
	var expected = findCase( '4x3_k3' );
	var result = runCase( '4x3_k3_input' );
	assert.equal( result.info, expected.INFO );
	assertArrayClose( result.Q, expected.Q, 1e-14, 'Q' );
});

test( 'dorgql: 3x3, K=3 (square)', function t() {
	var expected = findCase( '3x3_k3' );
	var result = runCase( '3x3_k3_input' );
	assert.equal( result.info, expected.INFO );
	assertArrayClose( result.Q, expected.Q, 1e-14, 'Q' );
});

test( 'dorgql: 4x2, K=2', function t() {
	var expected = findCase( '4x2_k2' );
	var result = runCase( '4x2_k2_input' );
	assert.equal( result.info, expected.INFO );
	assertArrayClose( result.Q, expected.Q, 1e-14, 'Q' );
});

test( 'dorgql: K=0 (identity-like columns)', function t() {
	var expected = findCase( 'k_zero' );
	var result = runCase( 'k_zero_input' );
	assert.equal( result.info, expected.INFO );
	assertArrayClose( result.Q, expected.Q, 1e-14, 'Q' );
});

test( 'dorgql: N=0 quick return', function t() {
	var A = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dorgql(3, 0, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dorgql: M=0, N=0 quick return', function t() {
	var A = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dorgql(0, 0, 0, A, 1, 0, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dorgql: 5x3, K=3 orthogonality check', function t() {
	var expected = findCase( '5x3_orthogonal' );
	var result = runCase( '5x3_k3_input' );
	var QtQ;
	var i;
	var j;
	var M = 5;
	var N = 3;

	assert.equal( result.info, expected.INFO );
	assertArrayClose( result.Q, expected.Q, 1e-14, 'Q' );

	// Verify Q^T * Q = I
	QtQ = computeQtQ( result.Q, M, N );
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			if ( i === j ) {
				assertClose( QtQ[ i + j * N ], 1.0, 1e-14, 'QtQ[' + i + ',' + j + ']' );
			} else {
				assertClose( QtQ[ i + j * N ], 0.0, 1e-14, 'QtQ[' + i + ',' + j + ']' );
			}
		}
	}
});

test( 'dorgql: 6x4, K=4', function t() {
	var expected = findCase( '6x4_k4' );
	var result = runCase( '6x4_k4_input' );
	assert.equal( result.info, expected.INFO );
	assertArrayClose( result.Q, expected.Q, 1e-14, 'Q' );
});

test( 'dorgql: 40x35, K=35 (blocked path)', function t() {
	var expected = findCase( '40x35_blocked' );
	var result = runCase( '40x35_input' );
	var QtQ;
	var M = 40;
	var N = 35;
	var i;
	var j;

	assert.equal( result.info, expected.INFO );
	assertArrayClose( result.Q, expected.Q, 1e-12, 'Q' );

	// Verify orthogonality: Q^T * Q = I
	QtQ = computeQtQ( result.Q, M, N );
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			if ( i === j ) {
				assertClose( QtQ[ i + j * N ], 1.0, 1e-12, 'QtQ[' + i + ',' + j + ']' );
			} else {
				assertClose( QtQ[ i + j * N ], 0.0, 1e-12, 'QtQ[' + i + ',' + j + ']' );
			}
		}
	}
});

test( 'dorgql: 40x35, K=34 (blocked path, N > K, exercises zero-fill)', function t() {
	var expected = findCase( '40x35_k34' );
	var result = runCase( '40x35_k34_input' );

	assert.equal( result.info, expected.INFO );
	assertArrayClose( result.Q, expected.Q, 1e-12, 'Q' );
});

test( 'dorgql: 4x3, K=1 (partial K)', function t() {
	var expected = findCase( '4x3_k1' );
	var result = runCase( '4x3_k1_input' );
	assert.equal( result.info, expected.INFO );
	assertArrayClose( result.Q, expected.Q, 1e-14, 'Q' );
});
