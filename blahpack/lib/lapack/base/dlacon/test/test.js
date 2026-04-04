/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlacon = require( './../lib/base.js' );

// FIXTURES //

var identity_3x3 = require( './fixtures/identity_3x3.json' );
var upper_tri_4x4 = require( './fixtures/upper_tri_4x4.json' );
var _1x1 = require( './fixtures/1x1.json' );
var diag_5x5 = require( './fixtures/diag_5x5.json' );
var dense_3x3 = require( './fixtures/dense_3x3.json' );

// FUNCTIONS //

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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
}

/**
* Matrix-vector multiply: y = A\_x or y = A^T\_x, column-major.
*
* @private
* @param {string} trans - `'N'` or `'T'`
* @param {number} n - matrix order
* @param {Float64Array} A - matrix (column-major)
* @param {Float64Array} x - input vector
* @returns {Float64Array} result vector
*/
function matvec( trans, n, A, x ) {
	var y = new Float64Array( n );
	var i;
	var j;
	for ( i = 0; i < n; i++ ) {
		for ( j = 0; j < n; j++ ) {
			if ( trans === 'N' ) {
				y[ i ] += A[ (j * n) + i ] * x[ j ];
			} else {
				y[ i ] += A[ (i * n) + j ] * x[ j ];
			}
		}
	}
	return y;
}

/**
* Run dlacon reverse-communication loop to estimate 1-norm of matrix A.
*
* @private
* @param {number} n - matrix order
* @param {Float64Array} A - matrix (column-major)
* @returns {Object} result with est and iterations
*/
function estimateNorm( n, A ) {
	var ISGN = new Int32Array( n );
	var KASE = new Int32Array( 1 );
	var iter = 0;
	var EST = new Float64Array( 1 );
	var yy;
	var v = new Float64Array( n );
	var x = new Float64Array( n );

	KASE[ 0 ] = 0;
	EST[ 0 ] = 0.0;

	while ( true ) { // eslint-disable-line no-constant-condition
		dlacon( n, v, 1, 0, x, 1, 0, ISGN, 1, 0, EST, KASE );
		if ( KASE[ 0 ] === 0 ) {
			break;
		}
		iter += 1;
		if ( iter > 20 ) {
			break;
		}
		if ( KASE[ 0 ] === 1 ) {
			yy = matvec( 'N', n, A, x );
			x.set( yy );
		} else {
			yy = matvec( 'T', n, A, x );
			x.set( yy );
		}
	}
	return {
		'est': EST[ 0 ],
		'iterations': iter
	};
}

// TESTS //

test( 'dlacon: main export is a function', function t() {
	assert.strictEqual( typeof dlacon, 'function' );
});

test( 'dlacon: 3x3 identity matrix (1-norm = 1)', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = identity_3x3;
	n = 3;
	A = new Float64Array([
		1,
		0,
		0,
		0,
		1,
		0,
		0,
		0,
		1
	]);
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacon: 4x4 upper triangular', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = upper_tri_4x4;
	n = 4;
	A = new Float64Array([
		1,
		0,
		0,
		0,
		2,
		1,
		0,
		0,
		3,
		0,
		1,
		0,
		4,
		0,
		0,
		1
	]);
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacon: 1x1 matrix', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = _1x1;
	n = 1;
	A = new Float64Array( [ 7 ] );
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacon: 5x5 diagonal matrix (1-norm = max|diag| = 5)', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = diag_5x5;
	n = 5;
	A = new Float64Array( n * n );
	A[ (0 * n) + 0 ] = 2.0;
	A[ (1 * n) + 1 ] = -5.0;
	A[ (2 * n) + 2 ] = 3.0;
	A[ (3 * n) + 3 ] = 1.0;
	A[ (4 * n) + 4 ] = 4.0;
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});

test( 'dlacon: 3x3 dense matrix (1-norm = 18)', function t() {
	var result;
	var tc;
	var n;
	var A;

	tc = dense_3x3;
	n = 3;
	A = new Float64Array([
		1,
		4,
		-7,
		-2,
		5,
		8,
		3,
		-6,
		9
	]);
	result = estimateNorm( n, A );
	assertClose( result.est, tc.est, 1e-10, 'est' );
	assert.strictEqual( result.iterations, tc.iterations, 'iterations' );
});
