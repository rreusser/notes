/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlatrs = require( './../lib/zlatrs.js' );


// TESTS //

test( 'zlatrs is a function', function t() {
	assert.strictEqual( typeof zlatrs, 'function', 'is a function' );
});

test( 'zlatrs has expected arity', function t() {
	assert.strictEqual( zlatrs.length, 2, 'has expected arity' );
});
