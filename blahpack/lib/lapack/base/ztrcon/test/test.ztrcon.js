/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrcon = require( './../lib/ztrcon.js' );


// TESTS //

test( 'ztrcon is a function', function t() {
	assert.strictEqual( typeof ztrcon, 'function', 'is a function' );
});

test( 'ztrcon has expected arity', function t() {
	assert.strictEqual( ztrcon.length, 2, 'has expected arity' );
});
