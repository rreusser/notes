/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dasum = require( './../lib/dasum.js' );


// TESTS //

test( 'dasum is a function', function t() {
	assert.strictEqual( typeof dasum, 'function', 'is a function' );
});

test( 'dasum has expected arity', function t() {
	assert.strictEqual( dasum.length, 3, 'has expected arity' );
});

test( 'dasum throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dasum( -1, 2, 1 );
	}, RangeError );
});
