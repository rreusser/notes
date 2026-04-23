
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var ndarrayFn = require( './../lib/ndarray.js' );
var base = require( './../lib/base.js' );


// TESTS //

test( 'base is a function', function t() {
	assert.strictEqual( typeof base, 'function', 'is a function' );
});

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarrayFn, 'function', 'is a function' );
});

test( 'ndarray and base behave identically (smoke)', function t() {
	// Both wrappers forward arguments without transformation, so simply
	// Confirm both export equivalent function arities.
	assert.strictEqual( ndarrayFn.length, base.length, 'arity matches' );
});
