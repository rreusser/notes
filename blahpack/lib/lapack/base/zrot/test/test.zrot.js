/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zrot = require( './../lib/zrot.js' );


// TESTS //

test( 'zrot is a function', function t() {
	assert.strictEqual( typeof zrot, 'function', 'is a function' );
});

test( 'zrot has expected arity', function t() {
	assert.strictEqual( zrot.length, 7, 'has expected arity' );
});

test( 'zrot throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zrot( -1, 2, 1, 2, 1, 2, 2 );
	}, RangeError );
});
