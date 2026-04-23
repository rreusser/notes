/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zsyconvf_rook = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zsyconvf_rook, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zsyconvf_rook.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'main export arity matches the layout wrapper', function t() {
	assert.strictEqual( zsyconvf_rook.length, 11, 'main arity' );
});

test( 'main export ndarray arity matches base signature', function t() {
	assert.strictEqual( zsyconvf_rook.ndarray.length, 13, 'ndarray arity' );
});
