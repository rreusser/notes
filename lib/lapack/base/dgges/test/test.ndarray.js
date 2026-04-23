

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgges = require( './../lib/base.js' );
var ndarrayFn = require( './../lib/ndarray.js' );


// FUNCTIONS //

/**
* Noop selection function.
*
* @private
* @returns {boolean} false
*/
function noop() {
	return false;
}


// TESTS //

test( 'base is a function', function t() {
	assert.strictEqual( typeof dgges, 'function', 'is a function' );
});

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarrayFn, 'function', 'is a function' );
});

test( 'ndarray throws TypeError for invalid jobvsl', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'invalid', 'compute-vectors', 'not-sorted', noop, 2, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 2 ), 1, 0, new Float64Array( 2 ), 1, 0, new Float64Array( 2 ), 1, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'ndarray throws TypeError for invalid jobvsr', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'compute-vectors', 'invalid', 'not-sorted', noop, 2, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 2 ), 1, 0, new Float64Array( 2 ), 1, 0, new Float64Array( 2 ), 1, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'ndarray throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'compute-vectors', 'compute-vectors', 'not-sorted', noop, -1, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 2 ), 1, 0, new Float64Array( 2 ), 1, 0, new Float64Array( 2 ), 1, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'ndarray computes Schur form for 2x2 diagonal', function t() {
	var ALPHAR = new Float64Array( 2 );
	var ALPHAI = new Float64Array( 2 );
	var result;
	var BETA = new Float64Array( 2 );
	var VSL = new Float64Array( 4 );
	var VSR = new Float64Array( 4 );
	var A = new Float64Array( [ 2, 0, 0, 3 ] );
	var B = new Float64Array( [ 1, 0, 0, 1 ] );

	result = ndarrayFn( 'compute-vectors', 'compute-vectors', 'not-sorted', noop, 2, A, 1, 2, 0, B, 1, 2, 0, ALPHAR, 1, 0, ALPHAI, 1, 0, BETA, 1, 0, VSL, 1, 2, 0, VSR, 1, 2, 0 ); // eslint-disable-line max-len
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.sdim, 0, 'sdim' );
});
