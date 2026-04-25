/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgbtf2 = require( './../lib/ndarray.js' );

// FIXTURES //

var tridiag_4x4 = require( './fixtures/tridiag_4x4.json' );
var kl1_ku2_3x3 = require( './fixtures/kl1_ku2_3x3.json' );
var n_zero = require( './fixtures/n_zero.json' );
var m_zero = require( './fixtures/m_zero.json' );
var one_by_one = require( './fixtures/one_by_one.json' );
var singular = require( './fixtures/singular.json' );
var tall_5x3 = require( './fixtures/tall_5x3.json' );
var pivot_2x2 = require( './fixtures/pivot_2x2.json' );

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
	assert.equal( actual.length, expected.length, msg + ': length mismatch: actual ' + actual.length + ' vs expected ' + expected.length ); // eslint-disable-line max-len
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
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

test( 'zgbtf2: main export is a function', function t() {
	assert.strictEqual( typeof zgbtf2, 'function' );
});

test( 'zgbtf2: 4x4 tridiagonal (KL=1, KU=1)', function t() {
	var IPIV;
	var info;
	var ABv;
	var tc;
	var AB;
	var i;

	tc = tridiag_4x4;
	AB = new Complex128Array( 4 * 4 );
	ABv = reinterpret( AB, 0 );
	ABv[ (2 + 0 * 4) * 2 ] = 4;
	ABv[ (2 + 0 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 0 * 4) * 2 ] = -1;
	ABv[ (3 + 0 * 4) * 2 + 1 ] = 0;
	ABv[ (1 + 1 * 4) * 2 ] = -1;
	ABv[ (1 + 1 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 1 * 4) * 2 ] = 4;
	ABv[ (2 + 1 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 1 * 4) * 2 ] = -1;
	ABv[ (3 + 1 * 4) * 2 + 1 ] = 0;
	ABv[ (1 + 2 * 4) * 2 ] = -1;
	ABv[ (1 + 2 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 2 * 4) * 2 ] = 4;
	ABv[ (2 + 2 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 2 * 4) * 2 ] = -1;
	ABv[ (3 + 2 * 4) * 2 + 1 ] = 0;
	ABv[ (1 + 3 * 4) * 2 ] = -1;
	ABv[ (1 + 3 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 3 * 4) * 2 ] = 4;
	ABv[ (2 + 3 * 4) * 2 + 1 ] = 1;
	IPIV = new Int32Array( 4 );
	info = zgbtf2( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( ABv ), tc.AB, 1e-10, 'AB' );
	for ( i = 0; i < tc.ipiv.length; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgbtf2: 3x3 with KL=1, KU=2', function t() {
	var IPIV;
	var info;
	var ABv;
	var tc;
	var AB;
	var i;

	tc = kl1_ku2_3x3;
	AB = new Complex128Array( 5 * 3 );
	ABv = reinterpret( AB, 0 );
	ABv[ (3 + 0 * 5) * 2 ] = 5;
	ABv[ (3 + 0 * 5) * 2 + 1 ] = 1;
	ABv[ (4 + 0 * 5) * 2 ] = 2;
	ABv[ (4 + 0 * 5) * 2 + 1 ] = 0;
	ABv[ (2 + 1 * 5) * 2 ] = 3;
	ABv[ (2 + 1 * 5) * 2 + 1 ] = 0;
	ABv[ (3 + 1 * 5) * 2 ] = 6;
	ABv[ (3 + 1 * 5) * 2 + 1 ] = 1;
	ABv[ (4 + 1 * 5) * 2 ] = 1;
	ABv[ (4 + 1 * 5) * 2 + 1 ] = 0;
	ABv[ (1 + 2 * 5) * 2 ] = 1;
	ABv[ (1 + 2 * 5) * 2 + 1 ] = 1;
	ABv[ (2 + 2 * 5) * 2 ] = 4;
	ABv[ (2 + 2 * 5) * 2 + 1 ] = 0;
	ABv[ (3 + 2 * 5) * 2 ] = 7;
	ABv[ (3 + 2 * 5) * 2 + 1 ] = 1;
	IPIV = new Int32Array( 3 );
	info = zgbtf2( 3, 3, 1, 2, AB, 1, 5, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( ABv ), tc.AB, 1e-10, 'AB' );
	for ( i = 0; i < tc.ipiv.length; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgbtf2: N=0 quick return', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;

	tc = n_zero;
	AB = new Complex128Array( 4 );
	IPIV = new Int32Array( 1 );
	info = zgbtf2( 3, 0, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'zgbtf2: M=0 quick return', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;

	tc = m_zero;
	AB = new Complex128Array( 4 );
	IPIV = new Int32Array( 1 );
	info = zgbtf2( 0, 3, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'zgbtf2: 1x1 matrix', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;

	tc = one_by_one;
	AB = new Complex128Array( [ 7, 2 ] );
	IPIV = new Int32Array( 1 );
	info = zgbtf2( 1, 1, 0, 0, AB, 1, 1, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( reinterpret( AB, 0 ) ), tc.AB, 1e-10, 'AB' );
	assert.strictEqual( IPIV[ 0 ], tc.ipiv[ 0 ] - 1, 'ipiv[0]' );
});

test( 'zgbtf2: singular matrix', function t() {
	var IPIV;
	var info;
	var ABv;
	var tc;
	var AB;

	tc = singular;
	AB = new Complex128Array( 2 * 2 );
	ABv = reinterpret( AB, 0 );
	ABv[ (0 + 1 * 2) * 2 ] = 1;
	ABv[ (0 + 1 * 2) * 2 + 1 ] = 1;
	IPIV = new Int32Array( 2 );
	info = zgbtf2( 2, 2, 0, 1, AB, 1, 2, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'zgbtf2: 5x3 tall matrix (M > N)', function t() {
	var IPIV;
	var info;
	var ABv;
	var tc;
	var AB;
	var i;

	tc = tall_5x3;
	AB = new Complex128Array( 4 * 3 );
	ABv = reinterpret( AB, 0 );
	ABv[ (2 + 0 * 4) * 2 ] = 4;
	ABv[ (2 + 0 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 0 * 4) * 2 ] = -1;
	ABv[ (3 + 0 * 4) * 2 + 1 ] = 0;
	ABv[ (1 + 1 * 4) * 2 ] = -1;
	ABv[ (1 + 1 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 1 * 4) * 2 ] = 4;
	ABv[ (2 + 1 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 1 * 4) * 2 ] = -1;
	ABv[ (3 + 1 * 4) * 2 + 1 ] = 0;
	ABv[ (1 + 2 * 4) * 2 ] = -1;
	ABv[ (1 + 2 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 2 * 4) * 2 ] = 4;
	ABv[ (2 + 2 * 4) * 2 + 1 ] = 1;
	ABv[ (3 + 2 * 4) * 2 ] = -1;
	ABv[ (3 + 2 * 4) * 2 + 1 ] = 0;
	IPIV = new Int32Array( 3 );
	info = zgbtf2( 5, 3, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( ABv ), tc.AB, 1e-10, 'AB' );
	for ( i = 0; i < tc.ipiv.length; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgbtf2: pivoting 2x2', function t() {
	var IPIV;
	var info;
	var ABv;
	var tc;
	var AB;
	var i;

	tc = pivot_2x2;
	AB = new Complex128Array( 4 * 2 );
	ABv = reinterpret( AB, 0 );
	ABv[ (2 + 0 * 4) * 2 ] = 1;
	ABv[ (2 + 0 * 4) * 2 + 1 ] = 0;
	ABv[ (3 + 0 * 4) * 2 ] = 3;
	ABv[ (3 + 0 * 4) * 2 + 1 ] = 0;
	ABv[ (1 + 1 * 4) * 2 ] = 2;
	ABv[ (1 + 1 * 4) * 2 + 1 ] = 0;
	ABv[ (2 + 1 * 4) * 2 ] = 4;
	ABv[ (2 + 1 * 4) * 2 + 1 ] = 0;
	IPIV = new Int32Array( 2 );
	info = zgbtf2( 2, 2, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( ABv ), tc.AB, 1e-10, 'AB' );
	for ( i = 0; i < tc.ipiv.length; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});
