/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlangt = require( './../lib/dlangt.js' );


// TESTS //

test( 'dlangt is a function', function t() {
	assert.strictEqual( typeof dlangt, 'function', 'is a function' );
});

test( 'dlangt has expected arity', function t() {
	assert.strictEqual( dlangt.length, 8, 'has expected arity' );
});

test( 'dlangt throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlangt( 2, -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
