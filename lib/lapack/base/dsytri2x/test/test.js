/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dsytri2x = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dsytri2x, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dsytri2x.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'ndarray method is a function', function t() {
	assert.strictEqual( typeof dsytri2x.ndarray, 'function', 'ndarray method is a function' ); // eslint-disable-line max-len
});
