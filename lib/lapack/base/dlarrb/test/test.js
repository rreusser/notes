
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlarrb = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlarrb, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dlarrb.ndarray, 'function', 'has ndarray method' );
});

test( 'main export and ndarray method have the expected arities', function t() {
	assert.strictEqual( dlarrb.length, 24, 'main has arity 24' );
	assert.strictEqual( dlarrb.ndarray.length, 30, 'ndarray has arity 30' );
});
