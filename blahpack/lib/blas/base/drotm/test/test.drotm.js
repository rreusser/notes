/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var drotm = require( './../lib/drotm.js' );


// TESTS //

test( 'drotm is a function', function t() {
	assert.strictEqual( typeof drotm, 'function', 'is a function' );
});

test( 'drotm has expected arity', function t() {
	assert.strictEqual( drotm.length, 6, 'has expected arity' );
});

test( 'drotm throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		drotm( -1, 2, 1, 2, 1, 2 );
	}, RangeError );
});
