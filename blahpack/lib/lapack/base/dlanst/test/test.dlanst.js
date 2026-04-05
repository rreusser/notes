/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlanst = require( './../lib/dlanst.js' );


// TESTS //

test( 'dlanst is a function', function t() {
	assert.strictEqual( typeof dlanst, 'function', 'is a function' );
});

test( 'dlanst has expected arity', function t() {
	assert.strictEqual( dlanst.length, 6, 'has expected arity' );
});

test( 'dlanst throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlanst( 2, -1, 2, 1, 2, 1 );
	}, RangeError );
});
