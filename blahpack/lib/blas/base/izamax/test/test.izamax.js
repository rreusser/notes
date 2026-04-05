/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var izamax = require( './../lib/izamax.js' );


// TESTS //

test( 'izamax is a function', function t() {
	assert.strictEqual( typeof izamax, 'function', 'is a function' );
});

test( 'izamax has expected arity', function t() {
	assert.strictEqual( izamax.length, 3, 'has expected arity' );
});

test( 'izamax throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		izamax( -1, 2, 1 );
	}, RangeError );
});
