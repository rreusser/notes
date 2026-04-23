/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgesc2 = require( './../lib/zgesc2.js' );


// TESTS //

test( 'zgesc2 is a function', function t() {
	assert.strictEqual( typeof zgesc2, 'function', 'is a function' );
});

test( 'zgesc2 has expected arity', function t() {
	assert.strictEqual( zgesc2.length, 10, 'has expected arity' );
});

test( 'zgesc2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgesc2( -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
