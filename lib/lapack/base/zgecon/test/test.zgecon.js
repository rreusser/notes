/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgecon = require( './../lib/zgecon.js' );


// TESTS //

test( 'zgecon is a function', function t() {
	assert.strictEqual( typeof zgecon, 'function', 'is a function' );
});

test( 'zgecon has expected arity', function t() {
	assert.strictEqual( zgecon.length, 2, 'has expected arity' );
});
