/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgebal = require( './../lib/zgebal.js' );


// TESTS //

test( 'zgebal is a function', function t() {
	assert.strictEqual( typeof zgebal, 'function', 'is a function' );
});

test( 'zgebal has expected arity', function t() {
	assert.strictEqual( zgebal.length, 6, 'has expected arity' );
});

test( 'zgebal throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgebal( 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
