/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlatdf = require( './../lib/zlatdf.js' );


// TESTS //

test( 'zlatdf is a function', function t() {
	assert.strictEqual( typeof zlatdf, 'function', 'is a function' );
});

test( 'zlatdf has expected arity', function t() {
	assert.strictEqual( zlatdf.length, 12, 'has expected arity' );
});

test( 'zlatdf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlatdf( 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
