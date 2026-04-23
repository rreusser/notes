/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlasyfRook = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlasyfRook, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dlasyfRook.ndarray, 'function', 'has ndarray method' );
});
