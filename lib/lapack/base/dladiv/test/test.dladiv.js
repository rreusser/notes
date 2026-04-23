/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dladiv = require( './../lib/dladiv.js' );


// TESTS //

test( 'dladiv is a function', function t() {
	assert.strictEqual( typeof dladiv, 'function', 'is a function' );
});

test( 'dladiv has expected arity', function t() {
	assert.strictEqual( dladiv.length, 5, 'has expected arity' );
});
