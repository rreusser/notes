/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlags2 = require( './../lib/zlags2.js' );


// TESTS //

test( 'zlags2 is a function', function t() {
	assert.strictEqual( typeof zlags2, 'function', 'is a function' );
});

test( 'zlags2 has expected arity', function t() {
	assert.strictEqual( zlags2.length, 7, 'has expected arity' );
});
