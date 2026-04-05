/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zunghr = require( './../lib/zunghr.js' );


// TESTS //

test( 'zunghr is a function', function t() {
	assert.strictEqual( typeof zunghr, 'function', 'is a function' );
});

test( 'zunghr has expected arity', function t() {
	assert.strictEqual( zunghr.length, 10, 'has expected arity' );
});

test( 'zunghr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zunghr( -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
