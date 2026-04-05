/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlatbs = require( './../lib/zlatbs.js' );


// TESTS //

test( 'zlatbs is a function', function t() {
	assert.strictEqual( typeof zlatbs, 'function', 'is a function' );
});

test( 'zlatbs has expected arity', function t() {
	assert.strictEqual( zlatbs.length, 2, 'has expected arity' );
});
