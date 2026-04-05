/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zdrscl = require( './../lib/zdrscl.js' );


// TESTS //

test( 'zdrscl is a function', function t() {
	assert.strictEqual( typeof zdrscl, 'function', 'is a function' );
});

test( 'zdrscl has expected arity', function t() {
	assert.strictEqual( zdrscl.length, 4, 'has expected arity' );
});

test( 'zdrscl throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zdrscl( -1, 2, 2, 1 );
	}, RangeError );
});
