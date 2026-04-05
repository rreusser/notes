/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zrotg = require( './../lib/zrotg.js' );


// TESTS //

test( 'zrotg is a function', function t() {
	assert.strictEqual( typeof zrotg, 'function', 'is a function' );
});

test( 'zrotg has expected arity', function t() {
	assert.strictEqual( zrotg.length, 2, 'has expected arity' );
});
