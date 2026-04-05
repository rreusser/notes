/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaqps = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlaqps, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dlaqps.ndarray, 'function', 'has ndarray method' );
});
