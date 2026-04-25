/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtprfb = require( './../lib/ndarray.js' );
var ndarrayFn = require( './../lib/ndarray.js' );


// TESTS //

test( 'base is a function', function t() {
	assert.strictEqual( typeof dtprfb, 'function', 'is a function' );
});

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarrayFn, 'function', 'is a function' );
});

test( 'ndarray throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'invalid', 'no-transpose', 'forward', 'columnwise', 2, 2, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 );
	}, TypeError );
});

test( 'ndarray throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'left', 'invalid', 'forward', 'columnwise', 2, 2, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 );
	}, TypeError );
});

test( 'ndarray throws TypeError for invalid direct', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'left', 'no-transpose', 'invalid', 'columnwise', 2, 2, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 );
	}, TypeError );
});

test( 'ndarray throws TypeError for invalid storev', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'left', 'no-transpose', 'forward', 'invalid', 2, 2, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 );
	}, TypeError );
});

test( 'ndarray throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'left', 'no-transpose', 'forward', 'columnwise', -1, 2, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 );
	}, RangeError );
});

test( 'ndarray throws RangeError for negative l', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'left', 'no-transpose', 'forward', 'columnwise', 2, 2, 2, -1, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 );
	}, RangeError );
});

test( 'ndarray returns early for M=0', function t() {
	var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	ndarrayFn( 'left', 'no-transpose', 'forward', 'columnwise', 0, 2, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, A, 1, 2, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 2, 0 );
	assert.strictEqual( A[ 0 ], 1.0 );
});
