/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zscal = require( './../lib/zscal.js' );


// TESTS //

test( 'zscal is a function', function t() {
	assert.strictEqual( typeof zscal, 'function', 'is a function' );
});

test( 'zscal has expected arity', function t() {
	assert.strictEqual( zscal.length, 4, 'has expected arity' );
});

test( 'zscal throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zscal( -1, 2, 2, 1 );
	}, RangeError );
});
