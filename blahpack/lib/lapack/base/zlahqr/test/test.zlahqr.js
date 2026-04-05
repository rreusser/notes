/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlahqr = require( './../lib/zlahqr.js' );


// TESTS //

test( 'zlahqr is a function', function t() {
	assert.strictEqual( typeof zlahqr, 'function', 'is a function' );
});

test( 'zlahqr has expected arity', function t() {
	assert.strictEqual( zlahqr.length, 2, 'has expected arity' );
});
