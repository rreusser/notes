/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var izmax1 = require( './../lib/izmax1.js' );


// TESTS //

test( 'izmax1 is a function', function t() {
	assert.strictEqual( typeof izmax1, 'function', 'is a function' );
});

test( 'izmax1 has expected arity', function t() {
	assert.strictEqual( izmax1.length, 3, 'has expected arity' );
});

test( 'izmax1 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		izmax1( -1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
