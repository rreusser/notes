/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var disnan = require( './../lib/disnan.js' );


// TESTS //

test( 'disnan is a function', function t() {
	assert.strictEqual( typeof disnan, 'function', 'is a function' );
});

test( 'disnan has expected arity', function t() {
	assert.strictEqual( disnan.length, 1, 'has expected arity' );
});
