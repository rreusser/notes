/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlapmt = require( './../lib/zlapmt.js' );


// TESTS //

test( 'zlapmt is a function', function t() {
	assert.strictEqual( typeof zlapmt, 'function', 'is a function' );
});

test( 'zlapmt has expected arity', function t() {
	assert.strictEqual( zlapmt.length, 7, 'has expected arity' );
});

test( 'zlapmt throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlapmt( 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});

test( 'zlapmt throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlapmt( 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});
