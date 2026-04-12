/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlarfgp = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlarfgp, 'function', 'main export is a function' );
	assert.strictEqual( dlarfgp.length, 7, 'main export has BLAS-style arity 7' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dlarfgp.ndarray, 'function', 'has ndarray method' );
	assert.strictEqual( dlarfgp.ndarray.length, 8, 'ndarray has arity 8' );
});

test( 'main and ndarray are distinct functions', function t() {
	assert.notStrictEqual( dlarfgp, dlarfgp.ndarray, 'ndarray is distinct from main' );
});
