/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlagv2 = require( './../lib/dlagv2.js' );


// TESTS //

test( 'dlagv2 is a function', function t() {
	assert.strictEqual( typeof dlagv2, 'function', 'is a function' );
});

test( 'dlagv2 has expected arity', function t() {
	assert.strictEqual( dlagv2.length, 7, 'has expected arity' );
});
