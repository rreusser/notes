/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlassq = require( './../lib/dlassq.js' );


// TESTS //

test( 'dlassq is a function', function t() {
	assert.strictEqual( typeof dlassq, 'function', 'is a function' );
});

test( 'dlassq has expected arity', function t() {
	assert.strictEqual( dlassq.length, 5, 'has expected arity' );
});

test( 'dlassq throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlassq( -1, 2, 1, 2, 2 );
	}, RangeError );
});
