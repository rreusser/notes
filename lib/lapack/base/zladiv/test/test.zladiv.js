/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zladiv = require( './../lib/zladiv.js' );


// TESTS //

test( 'zladiv is a function', function t() {
	assert.strictEqual( typeof zladiv, 'function', 'is a function' );
});

test( 'zladiv has expected arity', function t() {
	assert.strictEqual( zladiv.length, 6, 'has expected arity' );
});
