/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqge = require( './../lib/dlaqge.js' );


// TESTS //

test( 'dlaqge is a function', function t() {
	assert.strictEqual( typeof dlaqge, 'function', 'is a function' );
});

test( 'dlaqge has expected arity', function t() {
	assert.strictEqual( dlaqge.length, 11, 'has expected arity' );
});

test( 'dlaqge throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlaqge( -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, 2, 2, 2 );
	}, RangeError );
});

test( 'dlaqge throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaqge( new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1, 2, 1, 2, 2, 2 );
	}, RangeError );
});
