/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqgb = require( './../lib/dlaqgb.js' );


// TESTS //

test( 'dlaqgb is a function', function t() {
	assert.strictEqual( typeof dlaqgb, 'function', 'is a function' );
});

test( 'dlaqgb has expected arity', function t() {
	assert.strictEqual( dlaqgb.length, 13, 'has expected arity' );
});

test( 'dlaqgb throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlaqgb( -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 1, 2, 2, 2 );
	}, RangeError );
});

test( 'dlaqgb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaqgb( new Float64Array( 4 ), -1, 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 1, 2, 2, 2 );
	}, RangeError );
});
