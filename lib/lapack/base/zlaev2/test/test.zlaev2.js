/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaev2 = require( './../lib/zlaev2.js' );


// TESTS //

test( 'zlaev2 is a function', function t() {
	assert.strictEqual( typeof zlaev2, 'function', 'is a function' );
});

test( 'zlaev2 has expected arity', function t() {
	assert.strictEqual( zlaev2.length, 3, 'has expected arity' );
});
