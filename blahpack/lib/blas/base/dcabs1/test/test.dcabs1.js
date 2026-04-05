/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dcabs1 = require( './../lib/dcabs1.js' );


// TESTS //

test( 'dcabs1 is a function', function t() {
	assert.strictEqual( typeof dcabs1, 'function', 'is a function' );
});

test( 'dcabs1 has expected arity', function t() {
	assert.strictEqual( dcabs1.length, 1, 'has expected arity' );
});
