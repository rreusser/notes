/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlahr2 = require( './../lib/zlahr2.js' );


// TESTS //

test( 'zlahr2 is a function', function t() {
	assert.strictEqual( typeof zlahr2, 'function', 'is a function' );
});

test( 'zlahr2 has expected arity', function t() {
	assert.strictEqual( zlahr2.length, 18, 'has expected arity' );
});

test( 'zlahr2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlahr2( -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 1, 2, 2, 1, 2, new Float64Array( 4 ), 1, 1, 2, new Float64Array( 4 ), 1, 1, 2 );
	}, RangeError );
});

test( 'zlahr2 throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zlahr2( new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 1, 1, 2, 2, 1, 2, new Float64Array( 4 ), 1, 1, 2, new Float64Array( 4 ), 1, 1, 2 );
	}, RangeError );
});
