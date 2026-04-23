/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zsytri3x = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zsytri3x, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zsytri3x.ndarray, 'function', 'has ndarray method' );
});

test( 'ndarray method is a function', function t() {
	assert.strictEqual( typeof zsytri3x.ndarray, 'function', 'ndarray method is a function' ); // eslint-disable-line max-len
});
