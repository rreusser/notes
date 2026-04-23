/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlacon = require( './../lib/zlacon.js' );


// TESTS //

test( 'zlacon is a function', function t() {
	assert.strictEqual( typeof zlacon, 'function', 'is a function' );
});

test( 'zlacon has expected arity', function t() {
	assert.strictEqual( zlacon.length, 7, 'has expected arity' );
});

test( 'zlacon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlacon( -1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
