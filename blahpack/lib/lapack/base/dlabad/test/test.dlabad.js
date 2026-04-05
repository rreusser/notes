/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlabad = require( './../lib/dlabad.js' );


// TESTS //

test( 'dlabad is a function', function t() {
	assert.strictEqual( typeof dlabad, 'function', 'is a function' );
});

test( 'dlabad has expected arity', function t() {
	assert.strictEqual( dlabad.length, 2, 'has expected arity' );
});
