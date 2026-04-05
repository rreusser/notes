/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zcopy = require( './../lib/zcopy.js' );


// TESTS //

test( 'zcopy is a function', function t() {
	assert.strictEqual( typeof zcopy, 'function', 'is a function' );
});

test( 'zcopy has expected arity', function t() {
	assert.strictEqual( zcopy.length, 5, 'has expected arity' );
});

test( 'zcopy throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zcopy( -1, 2, 1, 2, 1 );
	}, RangeError );
});
