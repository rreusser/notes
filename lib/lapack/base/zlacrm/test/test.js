
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlacrm = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zlacrm, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zlacrm.ndarray, 'function', 'has ndarray method' );
});

test( 'main export and ndarray method have expected arities', function t() {
	assert.strictEqual( zlacrm.length, 10, 'main has expected arity' );
	assert.strictEqual( zlacrm.ndarray.length, 17, 'ndarray has expected arity' );
});
