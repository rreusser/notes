/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqgb = require( './../lib/zlaqgb.js' );


// TESTS //

test( 'zlaqgb is a function', function t() {
	assert.strictEqual( typeof zlaqgb, 'function', 'is a function' );
});

test( 'zlaqgb has expected arity', function t() {
	assert.strictEqual( zlaqgb.length, 13, 'has expected arity' );
});

test( 'zlaqgb throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlaqgb( -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 1, 2, 2, 2 );
	}, RangeError );
});

test( 'zlaqgb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaqgb( new Float64Array( 4 ), -1, 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 1, 2, 2, 2 );
	}, RangeError );
});
