/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dla_wwaddw = require( './../lib/dla_wwaddw.js' );


// TESTS //

test( 'dla_wwaddw is a function', function t() {
	assert.strictEqual( typeof dla_wwaddw, 'function', 'is a function' );
});

test( 'dla_wwaddw has expected arity', function t() {
	assert.strictEqual( dla_wwaddw.length, 4, 'has expected arity' );
});

test( 'dla_wwaddw throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dla_wwaddw( -1, 2, 2, 2 );
	}, RangeError );
});
