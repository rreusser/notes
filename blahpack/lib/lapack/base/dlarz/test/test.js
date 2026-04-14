/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlarz = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlarz, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dlarz.ndarray, 'function', 'has ndarray method' );
});

test( 'main and ndarray exports differ', function t() {
	assert.notStrictEqual( dlarz, dlarz.ndarray, 'main and ndarray are distinct functions' );
});
