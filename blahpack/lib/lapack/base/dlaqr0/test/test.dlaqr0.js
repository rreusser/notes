/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqr0 = require( './../lib/dlaqr0.js' );


// TESTS //

test( 'dlaqr0 is a function', function t() {
	assert.strictEqual( typeof dlaqr0, 'function', 'is a function' );
});

test( 'dlaqr0 has expected arity', function t() {
	assert.strictEqual( dlaqr0.length, 1, 'has expected arity' );
});
