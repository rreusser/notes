/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasrt = require( './../lib/dlasrt.js' );


// TESTS //

test( 'dlasrt is a function', function t() {
	assert.strictEqual( typeof dlasrt, 'function', 'is a function' );
});

test( 'dlasrt has expected arity', function t() {
	assert.strictEqual( dlasrt.length, 4, 'has expected arity' );
});

test( 'dlasrt throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlasrt( 2, -1, 2, 1 );
	}, RangeError );
});
