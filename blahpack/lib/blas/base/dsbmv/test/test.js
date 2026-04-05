/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dsbmv = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dsbmv, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dsbmv.ndarray, 'function', 'has ndarray method' );
});
