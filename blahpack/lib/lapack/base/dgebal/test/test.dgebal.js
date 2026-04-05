/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgebal = require( './../lib/dgebal.js' );


// TESTS //

test( 'dgebal is a function', function t() {
	assert.strictEqual( typeof dgebal, 'function', 'is a function' );
});

test( 'dgebal has expected arity', function t() {
	assert.strictEqual( dgebal.length, 6, 'has expected arity' );
});

test( 'dgebal throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgebal( 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
