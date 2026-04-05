/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasv2 = require( './../lib/dlasv2.js' );


// TESTS //

test( 'dlasv2 is a function', function t() {
	assert.strictEqual( typeof dlasv2, 'function', 'is a function' );
});

test( 'dlasv2 has expected arity', function t() {
	assert.strictEqual( dlasv2.length, 3, 'has expected arity' );
});
