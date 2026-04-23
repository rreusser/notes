/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlapmt = require( './../lib/dlapmt.js' );


// TESTS //

test( 'dlapmt is a function', function t() {
	assert.strictEqual( typeof dlapmt, 'function', 'is a function' );
});

test( 'dlapmt has expected arity', function t() {
	assert.strictEqual( dlapmt.length, 7, 'has expected arity' );
});

test( 'dlapmt throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlapmt( 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});

test( 'dlapmt throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlapmt( 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});
