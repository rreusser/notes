/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgetrf = require( '../../dgetrf/lib/base.js' );
var dgetrs = require( '../../dgetrs/lib/base.js' );
var dgerfs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dgerfs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	var relErr;
	if ( expected === 0.0 ) {
		assert.ok( Math.abs( actual ) <= tol, msg + ': expected ~0, got ' + actual );
		return;
	}
	relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
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
* Sets up a system: factorizes A, solves for initial X, returns all arrays.
*
* @private
* @param {string} trans - 'no-transpose' or 'transpose'
* @param {Array} aVals - column-major values for A (N*N)
* @param {Array} bVals - column-major values for B (N*NRHS)
* @param {number} N - matrix order
* @param {number} nrhs - number of right-hand sides
* @returns {Object} { A, AF, IPIV, B, X, FERR, BERR }
*/
function setupSystem( trans, aVals, bVals, N, nrhs ) {
	var IPIV = new Int32Array( N );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var info;
	var AF = new Float64Array( aVals );
	var A = new Float64Array( aVals );
	var B = new Float64Array( bVals );
	var X = new Float64Array( bVals );

	// Factorize AF = P*L*U
	info = dgetrf( N, N, AF, 1, N, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'dgetrf should succeed' );

	// Initial solve: op(AF) * X = B
	info = dgetrs( trans, N, nrhs, AF, 1, N, 0, IPIV, 1, 0, X, 1, N, 0 );
	assert.equal( info, 0, 'dgetrs should succeed' );

	return {
		'A': A,
		'AF': AF,
		'IPIV': IPIV,
		'B': B,
		'X': X,
		'FERR': FERR,
		'BERR': BERR
	};
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

test( 'dgerfs: basic_3x3', function t() {
	var info;
	var sys;
	var tc;

	tc = findCase( 'basic_3x3' );
	sys = setupSystem( 'no-transpose', [
		2,
		4,
		8,
		1,
		3,
		7,
		1,
		3,
		9
	], [ 1, 1, 1 ], 3, 1 );
	info = dgerfs( 'no-transpose', 3, 1, sys.A, 1, 3, 0, sys.AF, 1, 3, 0, sys.IPIV, 1, 0, sys.B, 1, 3, 0, sys.X, 1, 3, 0, sys.FERR, 1, 0, sys.BERR, 1, 0);
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( sys.X.subarray( 0, 3 ) ), tc.x, 1e-12, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR >= 0' );
	assert.ok( sys.BERR[ 0 ] < 1e-10, 'BERR is small' );
});

test( 'dgerfs: transpose_3x3', function t() {
	var info;
	var sys;
	var tc;

	tc = findCase( 'transpose_3x3' );
	sys = setupSystem( 'transpose', [
		2,
		4,
		8,
		1,
		3,
		7,
		1,
		3,
		9
	], [ 1, 1, 1 ], 3, 1 );
	info = dgerfs( 'transpose', 3, 1, sys.A, 1, 3, 0, sys.AF, 1, 3, 0, sys.IPIV, 1, 0, sys.B, 1, 3, 0, sys.X, 1, 3, 0, sys.FERR, 1, 0, sys.BERR, 1, 0);
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( sys.X.subarray( 0, 3 ) ), tc.x, 1e-12, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR >= 0' );
	assert.ok( sys.BERR[ 0 ] < 1e-10, 'BERR is small' );
});

test( 'dgerfs: multi_rhs', function t() {
	var info;
	var sys;
	var tc;

	tc = findCase( 'multi_rhs' );
	sys = setupSystem( 'no-transpose', [
		2,
		4,
		8,
		1,
		3,
		7,
		1,
		3,
		9
	], [ 1, 0, 0, 0, 1, 0 ], 3, 2 );
	info = dgerfs( 'no-transpose', 3, 2, sys.A, 1, 3, 0, sys.AF, 1, 3, 0, sys.IPIV, 1, 0, sys.B, 1, 3, 0, sys.X, 1, 3, 0, sys.FERR, 1, 0, sys.BERR, 1, 0);
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( sys.X.subarray( 0, 6 ) ), tc.x, 1e-12, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR[0] >= 0' );
	assert.ok( sys.FERR[ 1 ] >= 0.0, 'FERR[1] >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR[0] >= 0' );
	assert.ok( sys.BERR[ 1 ] >= 0.0, 'BERR[1] >= 0' );
});

test( 'dgerfs: n_zero', function t() {
	var IPIV;
	var FERR;
	var BERR;
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
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	info = dgerfs( 'no-transpose', 0, 1, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0);
	assert.equal( info, tc.info, 'info' );
	assert.equal( FERR[ 0 ], 0.0, 'FERR = 0 for N=0' );
	assert.equal( BERR[ 0 ], 0.0, 'BERR = 0 for N=0' );
});

test( 'dgerfs: nrhs_zero', function t() {
	var IPIV;
	var FERR;
	var BERR;
	var info;
	var tc;
	var AF;
	var A;
	var B;
	var X;

	tc = findCase( 'nrhs_zero' );
	A = new Float64Array( 9 );
	AF = new Float64Array( 9 );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( 3 );
	X = new Float64Array( 3 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	info = dgerfs( 'no-transpose', 3, 0, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0);
	assert.equal( info, tc.info, 'info' );
});

test( 'dgerfs: hilbert_3x3', function t() {
	var info;
	var sys;
	var tc;

	tc = findCase( 'hilbert_3x3' );
	sys = setupSystem( 'no-transpose', [
		1.0,
		0.5,
		1.0 / 3.0,
		0.5,
		1.0 / 3.0,
		0.25,
		1.0 / 3.0,
		0.25,
		0.2
	], [ 1, 1, 1 ], 3, 1 );
	info = dgerfs( 'no-transpose', 3, 1, sys.A, 1, 3, 0, sys.AF, 1, 3, 0, sys.IPIV, 1, 0, sys.B, 1, 3, 0, sys.X, 1, 3, 0, sys.FERR, 1, 0, sys.BERR, 1, 0);
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( sys.X.subarray( 0, 3 ) ), tc.x, 1e-10, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR >= 0' );
});

test( 'dgerfs: one_by_one', function t() {
	var info;
	var sys;
	var tc;

	tc = findCase( 'one_by_one' );
	sys = setupSystem( 'no-transpose', [ 5.0 ], [ 10.0 ], 1, 1 );
	info = dgerfs( 'no-transpose', 1, 1, sys.A, 1, 1, 0, sys.AF, 1, 1, 0, sys.IPIV, 1, 0, sys.B, 1, 1, 0, sys.X, 1, 1, 0, sys.FERR, 1, 0, sys.BERR, 1, 0);
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( sys.X.subarray( 0, 1 ) ), tc.x, 1e-14, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR >= 0' );
});
