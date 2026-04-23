/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dla_gerpvgrw = require( './../lib/dla_gerpvgrw.js' );


// TESTS //

test( 'dla_gerpvgrw is a function', function t() {
	assert.strictEqual( typeof dla_gerpvgrw, 'function', 'is a function' );
});

test( 'dla_gerpvgrw has expected arity', function t() {
	assert.strictEqual( dla_gerpvgrw.length, 6, 'has expected arity' );
});

test( 'dla_gerpvgrw throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dla_gerpvgrw( -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
