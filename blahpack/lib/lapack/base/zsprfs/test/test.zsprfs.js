/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsprfs = require( './../lib/zsprfs.js' );


// TESTS //

test( 'zsprfs is a function', function t() {
	assert.strictEqual( typeof zsprfs, 'function', 'is a function' );
});

test( 'zsprfs has expected arity', function t() {
	assert.strictEqual( zsprfs.length, 2, 'has expected arity' );
});
