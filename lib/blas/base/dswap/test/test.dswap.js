/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dswap = require( './../lib/dswap.js' );


// TESTS //

test( 'dswap is a function', function t() {
	assert.strictEqual( typeof dswap, 'function', 'is a function' );
});

test( 'dswap has expected arity', function t() {
	assert.strictEqual( dswap.length, 5, 'has expected arity' );
});

test( 'dswap throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dswap( -1, 2, 1, 2, 1 );
	}, RangeError );
});
