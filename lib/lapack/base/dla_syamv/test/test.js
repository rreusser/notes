
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dla_syamv = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dla_syamv, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dla_syamv.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'main export has expected arity (11)', function t() {
	assert.strictEqual( dla_syamv.length, 11, 'has expected arity' );
});

test( 'ndarray method has expected arity (14)', function t() {
	assert.strictEqual( dla_syamv.ndarray.length, 14, 'ndarray has expected arity' ); // eslint-disable-line max-len
});
