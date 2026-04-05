/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zswap = require( './../lib/zswap.js' );


// TESTS //

test( 'zswap is a function', function t() {
	assert.strictEqual( typeof zswap, 'function', 'is a function' );
});

test( 'zswap has expected arity', function t() {
	assert.strictEqual( zswap.length, 5, 'has expected arity' );
});

test( 'zswap throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zswap( -1, 2, 1, 2, 1 );
	}, RangeError );
});
