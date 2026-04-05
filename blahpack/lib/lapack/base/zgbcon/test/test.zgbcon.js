/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgbcon = require( './../lib/zgbcon.js' );


// TESTS //

test( 'zgbcon is a function', function t() {
	assert.strictEqual( typeof zgbcon, 'function', 'is a function' );
});

test( 'zgbcon has expected arity', function t() {
	assert.strictEqual( zgbcon.length, 2, 'has expected arity' );
});
