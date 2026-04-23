/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlar2v = require( './../lib/zlar2v.js' );


// TESTS //

test( 'zlar2v is a function', function t() {
	assert.strictEqual( typeof zlar2v, 'function', 'is a function' );
});

test( 'zlar2v has expected arity', function t() {
	assert.strictEqual( zlar2v.length, 8, 'has expected arity' );
});

test( 'zlar2v throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlar2v( -1, 2, 2, 2, 1, 2, 2, 1 );
	}, RangeError );
});
