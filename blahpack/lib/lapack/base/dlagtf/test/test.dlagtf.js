/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlagtf = require( './../lib/dlagtf.js' );


// TESTS //

test( 'dlagtf is a function', function t() {
	assert.strictEqual( typeof dlagtf, 'function', 'is a function' );
});

test( 'dlagtf has expected arity', function t() {
	assert.strictEqual( dlagtf.length, 13, 'has expected arity' );
});

test( 'dlagtf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlagtf( -1, 2, 1, 2, 2, 1, 2, 1, 2, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
