/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zaxpy = require( './../lib/zaxpy.js' );


// TESTS //

test( 'zaxpy is a function', function t() {
	assert.strictEqual( typeof zaxpy, 'function', 'is a function' );
});

test( 'zaxpy has expected arity', function t() {
	assert.strictEqual( zaxpy.length, 6, 'has expected arity' );
});

test( 'zaxpy throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zaxpy( -1, 2, 2, 1, 2, 1 );
	}, RangeError );
});
