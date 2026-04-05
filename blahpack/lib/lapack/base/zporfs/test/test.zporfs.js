/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zporfs = require( './../lib/zporfs.js' );


// TESTS //

test( 'zporfs is a function', function t() {
	assert.strictEqual( typeof zporfs, 'function', 'is a function' );
});

test( 'zporfs has expected arity', function t() {
	assert.strictEqual( zporfs.length, 2, 'has expected arity' );
});
