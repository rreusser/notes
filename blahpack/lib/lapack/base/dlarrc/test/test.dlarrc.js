/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarrc = require( './../lib/dlarrc.js' );


// TESTS //

test( 'dlarrc is a function', function t() {
	assert.strictEqual( typeof dlarrc, 'function', 'is a function' );
});

test( 'dlarrc has expected arity', function t() {
	assert.strictEqual( dlarrc.length, 7, 'has expected arity' );
});

test( 'dlarrc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarrc( 2, -1, 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
