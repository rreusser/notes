
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dla_geamv = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dla_geamv, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dla_geamv.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'main export has expected arity (12)', function t() {
	assert.strictEqual( dla_geamv.length, 12, 'has expected arity' );
});

test( 'ndarray method has expected arity (15)', function t() {
	assert.strictEqual( dla_geamv.ndarray.length, 15, 'ndarray has expected arity' ); // eslint-disable-line max-len
});
