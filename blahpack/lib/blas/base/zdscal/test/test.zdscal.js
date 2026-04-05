/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zdscal = require( './../lib/zdscal.js' );


// TESTS //

test( 'zdscal is a function', function t() {
	assert.strictEqual( typeof zdscal, 'function', 'is a function' );
});

test( 'zdscal has expected arity', function t() {
	assert.strictEqual( zdscal.length, 4, 'has expected arity' );
});

test( 'zdscal throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zdscal( -1, 2, 2, 1 );
	}, RangeError );
});
