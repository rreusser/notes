'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsytrf = require( './../../dsytrf/lib/base.js' );
var dsysvx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsysvx.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Helper to call dsysvx with standard col-major layout.
*/
function callDsysvx( fact, uplo, N, nrhs, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, lwork, IWORK ) {
	return dsysvx( fact, uplo, N, nrhs, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, B, 1, N, 0, X, 1, N, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, lwork, IWORK, 1, 0 );
}


// TESTS //

test( 'dsysvx: fact_n_upper', function t() {
	var tc = findCase( 'fact_n_upper' );
	// A = [4 2 1; 2 5 3; 1 3 6] upper triangle, col-major
	var A = new Float64Array( [ 4, 0, 0, 2, 5, 0, 1, 3, 6 ] );
	var AF = new Float64Array( 9 );
	var IPIV = new Int32Array( 3 );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var X = new Float64Array( 3 );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 3 );
	var info;

	info = callDsysvx( 'not-factored', 'upper', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( X ), tc.x, 1e-12, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-6, 'rcond' );
});

test( 'dsysvx: fact_n_lower', function t() {
	var tc = findCase( 'fact_n_lower' );
	// A = [4 2 1; 2 5 3; 1 3 6] lower triangle, col-major
	var A = new Float64Array( [ 4, 2, 1, 0, 5, 3, 0, 0, 6 ] );
	var AF = new Float64Array( 9 );
	var IPIV = new Int32Array( 3 );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var X = new Float64Array( 3 );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 3 );
	var info;

	info = callDsysvx( 'not-factored', 'lower', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( X ), tc.x, 1e-12, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-6, 'rcond' );
});

test( 'dsysvx: fact_f_upper (pre-factored)', function t() {
	var tc = findCase( 'fact_f_upper' );
	// A = [4 2 1; 2 5 3; 1 3 6] upper triangle, col-major
	var A = new Float64Array( [ 4, 0, 0, 2, 5, 0, 1, 3, 6 ] );
	var AF = new Float64Array( [ 4, 0, 0, 2, 5, 0, 1, 3, 6 ] );
	var IPIV = new Int32Array( 3 );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var X = new Float64Array( 3 );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 3 );
	var info;

	// Pre-factorize
	dsytrf( 'upper', 3, AF, 1, 3, 0, IPIV, 1, 0 );

	// Solve with FACT='factored'
	info = callDsysvx( 'factored', 'upper', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( X ), tc.x, 1e-12, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-6, 'rcond' );
});

test( 'dsysvx: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var AF = new Float64Array( 1 );
	var IPIV = new Int32Array( 1 );
	var B = new Float64Array( 1 );
	var X = new Float64Array( 1 );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Float64Array( 10 );
	var IWORK = new Int32Array( 1 );
	var info;

	info = callDsysvx( 'not-factored', 'upper', 0, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 10, IWORK );
	assert.equal( info, tc.info );
});

test( 'dsysvx: singular', function t() {
	var tc = findCase( 'singular' );
	// A = [1 0; 0 0] singular
	var A = new Float64Array( [ 1, 0, 0, 0 ] );
	var AF = new Float64Array( 4 );
	var IPIV = new Int32Array( 2 );
	var B = new Float64Array( [ 1, 1 ] );
	var X = new Float64Array( 2 );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 2 );
	var info;

	info = callDsysvx( 'not-factored', 'upper', 2, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK );
	assert.equal( info, tc.info );
	assert.equal( rcond[ 0 ], tc.rcond );
});

test( 'dsysvx: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var A = new Float64Array( [ 4, 0, 0, 2, 5, 0, 1, 3, 6 ] );
	var AF = new Float64Array( 9 );
	var IPIV = new Int32Array( 3 );
	var B = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var X = new Float64Array( 6 );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( 2 );
	var BERR = new Float64Array( 2 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 3 );
	var info;

	info = callDsysvx( 'not-factored', 'upper', 3, 2, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( X ), tc.x, 1e-12, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-6, 'rcond' );
});

test( 'dsysvx: ill_conditioned', function t() {
	var tc = findCase( 'ill_conditioned' );
	// Hilbert-like matrix, upper
	var A = new Float64Array( [ 1, 0, 0, 0.5, 1.0 / 3.0, 0, 1.0 / 3.0, 0.25, 0.2 ] );
	var AF = new Float64Array( 9 );
	var IPIV = new Int32Array( 3 );
	var B = new Float64Array( [ 1, 1, 1 ] );
	var X = new Float64Array( 3 );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 3 );
	var info;

	info = callDsysvx( 'not-factored', 'upper', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK );
	// For ill-conditioned, info may be N+1 (rcond < eps)
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( X ), tc.x, 1e-6, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-2, 'rcond' );
});

test( 'dsysvx: lwork_query', function t() {
	var tc = findCase( 'lwork_query' );
	var A = new Float64Array( 9 );
	var AF = new Float64Array( 9 );
	var IPIV = new Int32Array( 3 );
	var B = new Float64Array( 3 );
	var X = new Float64Array( 3 );
	var rcond = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Float64Array( 10 );
	var IWORK = new Int32Array( 3 );
	var info;

	info = callDsysvx( 'not-factored', 'upper', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, -1, IWORK );
	assert.equal( info, tc.info );
	// WORK[0] should contain optimal lwork
	assert.ok( WORK[ 0 ] >= 9, 'lwork_opt should be >= 3*N=9, got ' + WORK[ 0 ] );
});
