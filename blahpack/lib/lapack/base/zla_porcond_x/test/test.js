
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zla_porcond_x = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zla_porcond_x, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zla_porcond_x.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'main export has expected arity', function t() {
	assert.strictEqual( zla_porcond_x.length, 13, 'main arity is 13' );
});

test( 'ndarray method has expected arity', function t() {
	assert.strictEqual( zla_porcond_x.ndarray.length, 19, 'ndarray arity is 19' ); // eslint-disable-line max-len
});
