/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgerfs = require( './../lib/zgerfs.js' );


// TESTS //

test( 'zgerfs is a function', function t() {
	assert.strictEqual( typeof zgerfs, 'function', 'is a function' );
});

test( 'zgerfs has expected arity', function t() {
	assert.strictEqual( zgerfs.length, 2, 'has expected arity' );
});
