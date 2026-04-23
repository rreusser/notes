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
var ztprfb = require( './../lib/ztprfb.js' );


// TESTS //

test( 'ztprfb is a function', function t() {
	assert.strictEqual( typeof ztprfb, 'function', 'is a function' );
});

test( 'ztprfb has expected arity', function t() {
	assert.strictEqual( ztprfb.length, 19, 'has expected arity' );
});

test( 'ztprfb throws TypeError for invalid order', function t() {
	var V = new Complex128Array( 15 );
	var T = new Complex128Array( 9 );
	var A = new Complex128Array( 12 );
	var B = new Complex128Array( 20 );
	var W = new Complex128Array( 12 );
	assert.throws( function throws() {
		ztprfb( 'invalid', 'left', 'no-transpose', 'forward', 'columnwise', 5, 4, 3, 2, V, 5, T, 3, A, 3, B, 5, W, 3 );
	}, TypeError );
});

test( 'ztprfb throws TypeError for invalid side', function t() {
	var V = new Complex128Array( 15 );
	var T = new Complex128Array( 9 );
	var A = new Complex128Array( 12 );
	var B = new Complex128Array( 20 );
	var W = new Complex128Array( 12 );
	assert.throws( function throws() {
		ztprfb( 'column-major', 'invalid', 'no-transpose', 'forward', 'columnwise', 5, 4, 3, 2, V, 5, T, 3, A, 3, B, 5, W, 3 );
	}, TypeError );
});

test( 'ztprfb throws TypeError for invalid trans', function t() {
	var V = new Complex128Array( 15 );
	var T = new Complex128Array( 9 );
	var A = new Complex128Array( 12 );
	var B = new Complex128Array( 20 );
	var W = new Complex128Array( 12 );
	assert.throws( function throws() {
		ztprfb( 'column-major', 'left', 'invalid', 'forward', 'columnwise', 5, 4, 3, 2, V, 5, T, 3, A, 3, B, 5, W, 3 );
	}, TypeError );
});

test( 'ztprfb throws RangeError for negative M', function t() {
	var V = new Complex128Array( 15 );
	var T = new Complex128Array( 9 );
	var A = new Complex128Array( 12 );
	var B = new Complex128Array( 20 );
	var W = new Complex128Array( 12 );
	assert.throws( function throws() {
		ztprfb( 'column-major', 'left', 'no-transpose', 'forward', 'columnwise', -1, 4, 3, 2, V, 5, T, 3, A, 3, B, 5, W, 3 );
	}, RangeError );
});

test( 'ztprfb throws RangeError for negative N', function t() {
	var V = new Complex128Array( 15 );
	var T = new Complex128Array( 9 );
	var A = new Complex128Array( 12 );
	var B = new Complex128Array( 20 );
	var W = new Complex128Array( 12 );
	assert.throws( function throws() {
		ztprfb( 'column-major', 'left', 'no-transpose', 'forward', 'columnwise', 5, -1, 3, 2, V, 5, T, 3, A, 3, B, 5, W, 3 );
	}, RangeError );
});

test( 'ztprfb throws RangeError for negative K', function t() {
	var V = new Complex128Array( 15 );
	var T = new Complex128Array( 9 );
	var A = new Complex128Array( 12 );
	var B = new Complex128Array( 20 );
	var W = new Complex128Array( 12 );
	assert.throws( function throws() {
		ztprfb( 'column-major', 'left', 'no-transpose', 'forward', 'columnwise', 5, 4, -1, 2, V, 5, T, 3, A, 3, B, 5, W, 3 );
	}, RangeError );
});

test( 'ztprfb exposes ndarray method', function t() {
	var main = require( './../lib' );
	assert.strictEqual( typeof main.ndarray, 'function', 'has ndarray method' );
});
