/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaisnan = require( './../lib/dlaisnan.js' );


// TESTS //

test( 'dlaisnan is a function', function t() {
	assert.strictEqual( typeof dlaisnan, 'function', 'is a function' );
});

test( 'dlaisnan has expected arity', function t() {
	assert.strictEqual( dlaisnan.length, 2, 'has expected arity' );
});
