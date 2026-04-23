/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaln2 = require( './../lib/dlaln2.js' );


// TESTS //

test( 'dlaln2 is a function', function t() {
	assert.strictEqual( typeof dlaln2, 'function', 'is a function' );
});

test( 'dlaln2 has expected arity', function t() {
	assert.strictEqual( dlaln2.length, 15, 'has expected arity' );
});
