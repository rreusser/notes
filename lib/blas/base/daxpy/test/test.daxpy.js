/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var daxpy = require( './../lib/daxpy.js' );


// TESTS //

test( 'daxpy is a function', function t() {
	assert.strictEqual( typeof daxpy, 'function', 'is a function' );
});

test( 'daxpy has expected arity', function t() {
	assert.strictEqual( daxpy.length, 6, 'has expected arity' );
});

test( 'daxpy throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		daxpy( -1, 2, 2, 1, 2, 1 );
	}, RangeError );
});
