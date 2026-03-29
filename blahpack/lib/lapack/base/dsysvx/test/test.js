/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsytrf = require( './../../dsytrf/lib/base.js' );
var dsysvx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dsysvx.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
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
function callDsysvx( fact, uplo, N, nrhs, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, lwork, IWORK ) { // eslint-disable-line max-len
	return dsysvx( fact, uplo, N, nrhs, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, B, 1, N, 0, X, 1, N, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, lwork, IWORK, 1, 0 ); // eslint-disable-line max-len
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'dsysvx: fact_n_upper', function t() {
	var rcond;
	var IWORK;
	var IPIV;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var tc;
	var AF;
	var A;
	var B;
	var X;

	tc = findCase( 'fact_n_upper' );
	A = new Float64Array( [ 4, 0, 0, 2, 5, 0, 1, 3, 6 ] );
	AF = new Float64Array( 9 );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 1, 2, 3 ] );
	X = new Float64Array( 3 );
	rcond = new Float64Array( 1 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 100 );
	IWORK = new Int32Array( 3 );
	info = callDsysvx( 'not-factored', 'upper', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( X ), tc.x, 1e-12, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-6, 'rcond' );
});

test( 'dsysvx: fact_n_lower', function t() {
	var rcond;
	var IWORK;
	var IPIV;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var tc;
	var AF;
	var A;
	var B;
	var X;

	tc = findCase( 'fact_n_lower' );
	A = new Float64Array( [ 4, 2, 1, 0, 5, 3, 0, 0, 6 ] );
	AF = new Float64Array( 9 );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 1, 2, 3 ] );
	X = new Float64Array( 3 );
	rcond = new Float64Array( 1 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 100 );
	IWORK = new Int32Array( 3 );
	info = callDsysvx( 'not-factored', 'lower', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( X ), tc.x, 1e-12, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-6, 'rcond' );
});

test( 'dsysvx: fact_f_upper (pre-factored)', function t() {
	var rcond;
	var IWORK;
	var IPIV;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var tc;
	var AF;
	var A;
	var B;
	var X;

	tc = findCase( 'fact_f_upper' );
	A = new Float64Array( [ 4, 0, 0, 2, 5, 0, 1, 3, 6 ] );
	AF = new Float64Array( [ 4, 0, 0, 2, 5, 0, 1, 3, 6 ] );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 1, 2, 3 ] );
	X = new Float64Array( 3 );
	rcond = new Float64Array( 1 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 100 );
	IWORK = new Int32Array( 3 );
	dsytrf( 'upper', 3, AF, 1, 3, 0, IPIV, 1, 0 );
	info = callDsysvx( 'factored', 'upper', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( X ), tc.x, 1e-12, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-6, 'rcond' );
});

test( 'dsysvx: n_zero', function t() {
	var rcond;
	var IWORK;
	var IPIV;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var tc;
	var AF;
	var A;
	var B;
	var X;

	tc = findCase( 'n_zero' );
	A = new Float64Array( 1 );
	AF = new Float64Array( 1 );
	IPIV = new Int32Array( 1 );
	B = new Float64Array( 1 );
	X = new Float64Array( 1 );
	rcond = new Float64Array( 1 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 10 );
	IWORK = new Int32Array( 1 );
	info = callDsysvx( 'not-factored', 'upper', 0, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 10, IWORK ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'dsysvx: singular', function t() {
	var rcond;
	var IWORK;
	var IPIV;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var tc;
	var AF;
	var A;
	var B;
	var X;

	tc = findCase( 'singular' );
	A = new Float64Array( [ 1, 0, 0, 0 ] );
	AF = new Float64Array( 4 );
	IPIV = new Int32Array( 2 );
	B = new Float64Array( [ 1, 1 ] );
	X = new Float64Array( 2 );
	rcond = new Float64Array( 1 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 100 );
	IWORK = new Int32Array( 2 );
	info = callDsysvx( 'not-factored', 'upper', 2, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assert.equal( rcond[ 0 ], tc.rcond );
});

test( 'dsysvx: multi_rhs', function t() {
	var rcond;
	var IWORK;
	var IPIV;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var tc;
	var AF;
	var A;
	var B;
	var X;

	tc = findCase( 'multi_rhs' );
	A = new Float64Array( [ 4, 0, 0, 2, 5, 0, 1, 3, 6 ] );
	AF = new Float64Array( 9 );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	X = new Float64Array( 6 );
	rcond = new Float64Array( 1 );
	FERR = new Float64Array( 2 );
	BERR = new Float64Array( 2 );
	WORK = new Float64Array( 100 );
	IWORK = new Int32Array( 3 );
	info = callDsysvx( 'not-factored', 'upper', 3, 2, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( X ), tc.x, 1e-12, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-6, 'rcond' );
});

test( 'dsysvx: ill_conditioned', function t() {
	var rcond;
	var IWORK;
	var IPIV;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var tc;
	var AF;
	var A;
	var B;
	var X;

	tc = findCase( 'ill_conditioned' );
	A = new Float64Array( [ 1, 0, 0, 0.5, 1.0 / 3.0, 0, 1.0 / 3.0, 0.25, 0.2 ] );
	AF = new Float64Array( 9 );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 1, 1, 1 ] );
	X = new Float64Array( 3 );
	rcond = new Float64Array( 1 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 100 );
	IWORK = new Int32Array( 3 );
	info = callDsysvx( 'not-factored', 'upper', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, 100, IWORK ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( X ), tc.x, 1e-6, 'x' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-2, 'rcond' );
});

test( 'dsysvx: lwork_query', function t() {
	var rcond;
	var IWORK;
	var IPIV;
	var FERR;
	var BERR;
	var WORK;
	var info;
	var tc;
	var AF;
	var A;
	var B;
	var X;

	tc = findCase( 'lwork_query' );
	A = new Float64Array( 9 );
	AF = new Float64Array( 9 );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( 3 );
	X = new Float64Array( 3 );
	rcond = new Float64Array( 1 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	WORK = new Float64Array( 10 );
	IWORK = new Int32Array( 3 );
	info = callDsysvx( 'not-factored', 'upper', 3, 1, A, AF, IPIV, B, X, rcond, FERR, BERR, WORK, -1, IWORK ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assert.ok( WORK[ 0 ] >= 9, 'lwork_opt should be >= 3*N=9, got ' + WORK[ 0 ] );
});
