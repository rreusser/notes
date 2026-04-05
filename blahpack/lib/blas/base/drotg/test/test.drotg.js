/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var drotg = require( './../lib/drotg.js' );


// TESTS //

test( 'drotg is a function', function t() {
	assert.strictEqual( typeof drotg, 'function', 'is a function' );
});

test( 'drotg has expected arity', function t() {
	assert.strictEqual( drotg.length, 4, 'has expected arity' );
});
