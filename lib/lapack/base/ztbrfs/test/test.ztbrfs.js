/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztbrfs = require( './../lib/ztbrfs.js' );


// TESTS //

test( 'ztbrfs is a function', function t() {
	assert.strictEqual( typeof ztbrfs, 'function', 'is a function' );
});

test( 'ztbrfs has expected arity', function t() {
	assert.strictEqual( ztbrfs.length, 1, 'has expected arity' );
});
