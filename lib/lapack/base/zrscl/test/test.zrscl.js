/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zrscl = require( './../lib/zrscl.js' );


// TESTS //

test( 'zrscl is a function', function t() {
	assert.strictEqual( typeof zrscl, 'function', 'is a function' );
});

test( 'zrscl has expected arity', function t() {
	assert.strictEqual( zrscl.length, 4, 'has expected arity' );
});

test( 'zrscl throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zrscl( -1, 2, 2, 1 );
	}, RangeError );
});
