/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dzasum = require( './../lib/dzasum.js' );


// TESTS //

test( 'dzasum is a function', function t() {
	assert.strictEqual( typeof dzasum, 'function', 'is a function' );
});

test( 'dzasum has expected arity', function t() {
	assert.strictEqual( dzasum.length, 4, 'has expected arity' );
});

test( 'dzasum throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dzasum( -1, 2, 1, 2 );
	}, RangeError );
});
