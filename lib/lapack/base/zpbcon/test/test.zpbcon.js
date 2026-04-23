/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpbcon = require( './../lib/zpbcon.js' );


// TESTS //

test( 'zpbcon is a function', function t() {
	assert.strictEqual( typeof zpbcon, 'function', 'is a function' );
});

test( 'zpbcon has expected arity', function t() {
	assert.strictEqual( zpbcon.length, 2, 'has expected arity' );
});
