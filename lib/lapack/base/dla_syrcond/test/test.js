
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dla_syrcond = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dla_syrcond, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dla_syrcond.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'main export has expected arity (18)', function t() {
	assert.strictEqual( dla_syrcond.length, 18, 'has expected arity' );
});

test( 'ndarray method has expected arity (23)', function t() {
	assert.strictEqual( dla_syrcond.ndarray.length, 23, 'ndarray has expected arity' ); // eslint-disable-line max-len
});
