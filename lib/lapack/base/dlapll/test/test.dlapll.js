/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlapll = require( './../lib/dlapll.js' );


// TESTS //

test( 'dlapll is a function', function t() {
	assert.strictEqual( typeof dlapll, 'function', 'is a function' );
});

test( 'dlapll has expected arity', function t() {
	assert.strictEqual( dlapll.length, 6, 'has expected arity' );
});

test( 'dlapll throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlapll( -1, 2, 1, 2, 1, 2 );
	}, RangeError );
});
