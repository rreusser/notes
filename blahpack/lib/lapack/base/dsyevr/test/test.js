
'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsyevr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsyevr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch (actual=' + actual.length + ', expected=' + expected.length + ')' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* 4x4 symmetric matrix (column-major flat):
*   [10    1     0.5   0.25]
*   [1     8     0.5   0   ]
*   [0.5   0.5   12    1   ]
*   [0.25  0     1     6   ]
*
* Stored as lower triangle for 'lower', upper for 'upper'.
*/
function symMatrix4Lower() {
	// Full column-major, but only lower triangle matters for UPLO='L'
	return new Float64Array([
		10, 1, 0.5, 0.25,
		1, 8, 0.5, 0,
		0.5, 0.5, 12, 1,
		0.25, 0, 1, 6
	]);
}

function symMatrix4Upper() {
	// Full column-major, but only upper triangle matters for UPLO='U'
	return new Float64Array([
		10, 1, 0.5, 0.25,
		1, 8, 0.5, 0,
		0.5, 0.5, 12, 1,
		0.25, 0, 1, 6
	]);
}

/**
* Verify eigenvector property: A*v = lambda*v for each eigenpair.
* Aorig is N x N symmetric (Float64Array column-major), Z is N x M, w is eigenvalues.
*/
function verifyEigenpairs( Aorig, N, w, Z, M, tol, msg ) {
	var Avec;
	var v;
	var i;
	var j;
	var k;
	var err;
	var nrm;

	for ( k = 0; k < M; k++ ) {
		Avec = new Float64Array( N );
		v = new Float64Array( N );

		// Extract eigenvector k (column k of Z)
		for ( i = 0; i < N; i++ ) {
			v[ i ] = Z[ i + k * N ];
		}

		// Compute A*v
		for ( i = 0; i < N; i++ ) {
			Avec[ i ] = 0.0;
			for ( j = 0; j < N; j++ ) {
				Avec[ i ] += Aorig[ i + j * N ] * v[ j ];
			}
		}

		// Check ||A*v - lambda*v|| / (||v|| * |lambda|)
		err = 0.0;
		nrm = 0.0;
		for ( i = 0; i < N; i++ ) {
			err += ( Avec[ i ] - w[ k ] * v[ i ] ) * ( Avec[ i ] - w[ k ] * v[ i ] );
			nrm += v[ i ] * v[ i ];
		}
		err = Math.sqrt( err );
		nrm = Math.sqrt( nrm );
		assert.ok( err / ( nrm * Math.max( Math.abs( w[ k ] ), 1.0 ) ) < tol, msg + ': eigenpair ' + k + ' residual too large (' + err + ')' );
	}
}


// TESTS //

test( 'dsyevr: V_A_L - compute all eigenvectors, lower', function t() {
	var ISUPPZ;
	var IWORK;
	var WORK;
	var info;
	var out;
	var tc;
	var A;
	var w;
	var Z;
	var N;

	tc = findCase( 'V_A_L' );
	N = tc.N;
	A = symMatrix4Lower();
	w = new Float64Array( N );
	Z = new Float64Array( N * N );
	ISUPPZ = new Int32Array( 2 * N );
	WORK = new Float64Array( 26 * N );
	IWORK = new Int32Array( 10 * N );
	out = {};

	info = dsyevr( 'compute-vectors', 'all', 'lower', N, A, 1, N, 0, 0.0, 0.0, 0, 0, 0.0, out, w, 1, 0, Z, 1, N, 0, ISUPPZ, 1, 0, WORK, 1, 0, 26 * N, IWORK, 1, 0, 10 * N );

	assert.equal( info, 0 );
	assert.equal( out.M, tc.M );
	assertArrayClose( Array.from( w.subarray( 0, out.M ) ), tc.w, 1e-12, 'eigenvalues' );
	verifyEigenpairs( symMatrix4Lower(), N, w, Z, out.M, 1e-12, 'eigenpairs' );
});

test( 'dsyevr: V_A_U - compute all eigenvectors, upper', function t() {
	var ISUPPZ;
	var IWORK;
	var WORK;
	var info;
	var out;
	var tc;
	var A;
	var w;
	var Z;
	var N;

	tc = findCase( 'V_A_U' );
	N = tc.N;
	A = symMatrix4Upper();
	w = new Float64Array( N );
	Z = new Float64Array( N * N );
	ISUPPZ = new Int32Array( 2 * N );
	WORK = new Float64Array( 26 * N );
	IWORK = new Int32Array( 10 * N );
	out = {};

	info = dsyevr( 'compute-vectors', 'all', 'upper', N, A, 1, N, 0, 0.0, 0.0, 0, 0, 0.0, out, w, 1, 0, Z, 1, N, 0, ISUPPZ, 1, 0, WORK, 1, 0, 26 * N, IWORK, 1, 0, 10 * N );

	assert.equal( info, 0 );
	assert.equal( out.M, tc.M );
	assertArrayClose( Array.from( w.subarray( 0, out.M ) ), tc.w, 1e-12, 'eigenvalues' );
	verifyEigenpairs( symMatrix4Upper(), N, w, Z, out.M, 1e-12, 'eigenpairs' );
});

test( 'dsyevr: N_A_L - eigenvalues only, all, lower', function t() {
	var ISUPPZ;
	var IWORK;
	var WORK;
	var info;
	var out;
	var tc;
	var A;
	var w;
	var Z;
	var N;

	tc = findCase( 'N_A_L' );
	N = tc.N;
	A = symMatrix4Lower();
	w = new Float64Array( N );
	Z = new Float64Array( 1 );
	ISUPPZ = new Int32Array( 2 * N );
	WORK = new Float64Array( 26 * N );
	IWORK = new Int32Array( 10 * N );
	out = {};

	info = dsyevr( 'no-vectors', 'all', 'lower', N, A, 1, N, 0, 0.0, 0.0, 0, 0, 0.0, out, w, 1, 0, Z, 1, 1, 0, ISUPPZ, 1, 0, WORK, 1, 0, 26 * N, IWORK, 1, 0, 10 * N );

	assert.equal( info, 0 );
	assert.equal( out.M, tc.M );
	assertArrayClose( Array.from( w.subarray( 0, out.M ) ), tc.w, 1e-12, 'eigenvalues' );
});

test( 'dsyevr: V_V_L - value range, lower, compute vectors', function t() {
	var ISUPPZ;
	var IWORK;
	var WORK;
	var info;
	var out;
	var tc;
	var A;
	var w;
	var Z;
	var N;

	tc = findCase( 'V_V_L' );
	N = tc.N;
	A = symMatrix4Lower();
	w = new Float64Array( N );
	Z = new Float64Array( N * N );
	ISUPPZ = new Int32Array( 2 * N );
	WORK = new Float64Array( 26 * N );
	IWORK = new Int32Array( 10 * N );
	out = {};

	info = dsyevr( 'compute-vectors', 'value', 'lower', N, A, 1, N, 0, 7.0, 11.0, 0, 0, 0.0, out, w, 1, 0, Z, 1, N, 0, ISUPPZ, 1, 0, WORK, 1, 0, 26 * N, IWORK, 1, 0, 10 * N );

	assert.equal( info, 0 );
	assert.equal( out.M, tc.M );
	assertArrayClose( Array.from( w.subarray( 0, out.M ) ), tc.w, 1e-12, 'eigenvalues' );
	verifyEigenpairs( symMatrix4Lower(), N, w, Z, out.M, 1e-12, 'eigenpairs' );
});

test( 'dsyevr: V_I_L - index range, lower, compute vectors', function t() {
	var ISUPPZ;
	var IWORK;
	var WORK;
	var info;
	var out;
	var tc;
	var A;
	var w;
	var Z;
	var N;

	tc = findCase( 'V_I_L' );
	N = tc.N;
	A = symMatrix4Lower();
	w = new Float64Array( N );
	Z = new Float64Array( N * N );
	ISUPPZ = new Int32Array( 2 * N );
	WORK = new Float64Array( 26 * N );
	IWORK = new Int32Array( 10 * N );
	out = {};

	info = dsyevr( 'compute-vectors', 'index', 'lower', N, A, 1, N, 0, 0.0, 0.0, 2, 3, 0.0, out, w, 1, 0, Z, 1, N, 0, ISUPPZ, 1, 0, WORK, 1, 0, 26 * N, IWORK, 1, 0, 10 * N );

	assert.equal( info, 0 );
	assert.equal( out.M, tc.M );
	assertArrayClose( Array.from( w.subarray( 0, out.M ) ), tc.w, 1e-12, 'eigenvalues' );
	verifyEigenpairs( symMatrix4Lower(), N, w, Z, out.M, 1e-12, 'eigenpairs' );
});

test( 'dsyevr: N0 - N=0', function t() {
	var ISUPPZ;
	var IWORK;
	var WORK;
	var info;
	var out;
	var A;
	var w;
	var Z;

	A = new Float64Array( 1 );
	w = new Float64Array( 1 );
	Z = new Float64Array( 1 );
	ISUPPZ = new Int32Array( 1 );
	WORK = new Float64Array( 1 );
	IWORK = new Int32Array( 1 );
	out = {};

	info = dsyevr( 'compute-vectors', 'all', 'lower', 0, A, 1, 1, 0, 0.0, 0.0, 0, 0, 0.0, out, w, 1, 0, Z, 1, 1, 0, ISUPPZ, 1, 0, WORK, 1, 0, 1, IWORK, 1, 0, 1 );

	assert.equal( info, 0 );
	assert.equal( out.M, 0 );
});

test( 'dsyevr: N1 - N=1', function t() {
	var ISUPPZ;
	var IWORK;
	var WORK;
	var info;
	var out;
	var tc;
	var A;
	var w;
	var Z;

	tc = findCase( 'N1' );
	A = new Float64Array( [ 5.0 ] );
	w = new Float64Array( 1 );
	Z = new Float64Array( 1 );
	ISUPPZ = new Int32Array( 2 );
	WORK = new Float64Array( 26 );
	IWORK = new Int32Array( 10 );
	out = {};

	info = dsyevr( 'compute-vectors', 'all', 'lower', 1, A, 1, 1, 0, 0.0, 0.0, 0, 0, 0.0, out, w, 1, 0, Z, 1, 1, 0, ISUPPZ, 1, 0, WORK, 1, 0, 26, IWORK, 1, 0, 10 );

	assert.equal( info, 0 );
	assert.equal( out.M, 1 );
	assertClose( w[ 0 ], 5.0, 1e-14, 'eigenvalue' );
	assertClose( Z[ 0 ], 1.0, 1e-14, 'eigenvector' );
});

test( 'dsyevr: N_V_U - eigenvalues only, value range, upper', function t() {
	var ISUPPZ;
	var IWORK;
	var WORK;
	var info;
	var out;
	var tc;
	var A;
	var w;
	var Z;
	var N;

	tc = findCase( 'N_V_U' );
	N = tc.N;
	A = symMatrix4Upper();
	w = new Float64Array( N );
	Z = new Float64Array( 1 );
	ISUPPZ = new Int32Array( 2 * N );
	WORK = new Float64Array( 26 * N );
	IWORK = new Int32Array( 10 * N );
	out = {};

	info = dsyevr( 'no-vectors', 'value', 'upper', N, A, 1, N, 0, 7.0, 11.0, 0, 0, 0.0, out, w, 1, 0, Z, 1, 1, 0, ISUPPZ, 1, 0, WORK, 1, 0, 26 * N, IWORK, 1, 0, 10 * N );

	assert.equal( info, 0 );
	assert.equal( out.M, tc.M );
	assertArrayClose( Array.from( w.subarray( 0, out.M ) ), tc.w, 1e-12, 'eigenvalues' );
});

test( 'dsyevr: N_I_L - eigenvalues only, index range, lower', function t() {
	var ISUPPZ;
	var IWORK;
	var WORK;
	var info;
	var out;
	var tc;
	var A;
	var w;
	var Z;
	var N;

	tc = findCase( 'N_I_L' );
	N = tc.N;
	A = symMatrix4Lower();
	w = new Float64Array( N );
	Z = new Float64Array( 1 );
	ISUPPZ = new Int32Array( 2 * N );
	WORK = new Float64Array( 26 * N );
	IWORK = new Int32Array( 10 * N );
	out = {};

	info = dsyevr( 'no-vectors', 'index', 'lower', N, A, 1, N, 0, 0.0, 0.0, 1, 2, 0.0, out, w, 1, 0, Z, 1, 1, 0, ISUPPZ, 1, 0, WORK, 1, 0, 26 * N, IWORK, 1, 0, 10 * N );

	assert.equal( info, 0 );
	assert.equal( out.M, tc.M );
	assertArrayClose( Array.from( w.subarray( 0, out.M ) ), tc.w, 1e-12, 'eigenvalues' );
});
