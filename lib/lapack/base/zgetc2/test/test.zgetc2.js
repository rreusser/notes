/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgetc2 = require( './../lib/zgetc2.js' );


// TESTS //

test( 'zgetc2 is a function', function t() {
	assert.strictEqual( typeof zgetc2, 'function', 'is a function' );
});

test( 'zgetc2 has expected arity', function t() {
	assert.strictEqual( zgetc2.length, 7, 'has expected arity' );
});

test( 'zgetc2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgetc2( -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
