/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zsytf2Rook = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zsytf2Rook, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zsytf2Rook.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'main export and ndarray are distinct functions', function t() {
	assert.notStrictEqual( zsytf2Rook, zsytf2Rook.ndarray, 'are distinct' );
	assert.strictEqual( typeof zsytf2Rook.ndarray, 'function', 'is a function' );
});
