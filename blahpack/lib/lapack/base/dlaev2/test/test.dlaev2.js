/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaev2 = require( './../lib/dlaev2.js' );


// TESTS //

test( 'dlaev2 is a function', function t() {
	assert.strictEqual( typeof dlaev2, 'function', 'is a function' );
});

test( 'dlaev2 has expected arity', function t() {
	assert.strictEqual( dlaev2.length, 3, 'has expected arity' );
});
