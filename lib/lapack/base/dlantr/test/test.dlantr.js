/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlantr = require( './../lib/dlantr.js' );


// TESTS //

test( 'dlantr is a function', function t() {
	assert.strictEqual( typeof dlantr, 'function', 'is a function' );
});

test( 'dlantr has expected arity', function t() {
	assert.strictEqual( dlantr.length, 2, 'has expected arity' );
});
