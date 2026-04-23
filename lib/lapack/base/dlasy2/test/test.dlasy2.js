/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasy2 = require( './../lib/dlasy2.js' );


// TESTS //

test( 'dlasy2 is a function', function t() {
	assert.strictEqual( typeof dlasy2, 'function', 'is a function' );
});

test( 'dlasy2 has expected arity', function t() {
	assert.strictEqual( dlasy2.length, 15, 'has expected arity' );
});
