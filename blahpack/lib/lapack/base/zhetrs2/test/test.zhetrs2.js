/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhetrs2 = require( './../lib/zhetrs2.js' );


// TESTS //

test( 'zhetrs2 is a function', function t() {
	assert.strictEqual( typeof zhetrs2, 'function', 'is a function' );
});

test( 'zhetrs2 has expected arity', function t() {
	assert.strictEqual( zhetrs2.length, 4, 'has expected arity' );
});
