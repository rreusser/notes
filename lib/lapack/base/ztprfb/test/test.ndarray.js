/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ndarray = require( './../lib/ndarray.js' );


// TESTS //

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarray, 'function', 'is a function' );
});

test( 'ndarray throws TypeError for invalid side', function t() {
	var V = new Complex128Array( 15 );
	var T = new Complex128Array( 9 );
	var A = new Complex128Array( 12 );
	var B = new Complex128Array( 20 );
	var W = new Complex128Array( 12 );
	assert.throws( function throws() {
		ndarray( 'invalid', 'no-transpose', 'forward', 'columnwise', 5, 4, 3, 2, V, 1, 5, 0, T, 1, 3, 0, A, 1, 3, 0, B, 1, 5, 0, W, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray throws TypeError for invalid trans', function t() {
	var V = new Complex128Array( 15 );
	var T = new Complex128Array( 9 );
	var A = new Complex128Array( 12 );
	var B = new Complex128Array( 20 );
	var W = new Complex128Array( 12 );
	assert.throws( function throws() {
		ndarray( 'left', 'invalid', 'forward', 'columnwise', 5, 4, 3, 2, V, 1, 5, 0, T, 1, 3, 0, A, 1, 3, 0, B, 1, 5, 0, W, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray returns B (no-op for zero K)', function t() {
	var view;
	var out;
	var V;
	var T;
	var A;
	var B;
	var W;
	var i;

	V = new Complex128Array( 15 );
	T = new Complex128Array( 9 );
	A = new Complex128Array( 12 );
	B = new Complex128Array( 20 );
	W = new Complex128Array( 12 );
	out = ndarray( 'left', 'no-transpose', 'forward', 'columnwise', 5, 4, 0, 0, V, 1, 5, 0, T, 1, 3, 0, A, 1, 3, 0, B, 1, 5, 0, W, 1, 3, 0 );
	assert.strictEqual( out, B );
	view = reinterpret( B, 0 );
	for ( i = 0; i < view.length; i++ ) {
		assert.strictEqual( view[ i ], 0 );
	}
});
