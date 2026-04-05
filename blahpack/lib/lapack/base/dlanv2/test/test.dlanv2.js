/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlanv2 = require( './../lib/dlanv2.js' );


// TESTS //

test( 'dlanv2 is a function', function t() {
	assert.strictEqual( typeof dlanv2, 'function', 'is a function' );
});

test( 'dlanv2 has expected arity', function t() {
	assert.strictEqual( dlanv2.length, 4, 'has expected arity' );
});
