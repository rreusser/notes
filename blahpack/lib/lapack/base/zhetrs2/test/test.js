/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zhetrs2 = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zhetrs2, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zhetrs2.ndarray, 'function', 'has ndarray method' );
});
