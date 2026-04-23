/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dstev = require( './../lib/dstev.js' );


// TESTS //

test( 'dstev is a function', function t() {
	assert.strictEqual( typeof dstev, 'function', 'is a function' );
});

test( 'dstev has expected arity', function t() {
	assert.strictEqual( dstev.length, 10, 'has expected arity' );
});

test( 'dstev throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dstev( 2, -1, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
