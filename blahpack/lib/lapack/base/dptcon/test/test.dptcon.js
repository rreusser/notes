/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dptcon = require( './../lib/dptcon.js' );


// TESTS //

test( 'dptcon is a function', function t() {
	assert.strictEqual( typeof dptcon, 'function', 'is a function' );
});

test( 'dptcon has expected arity', function t() {
	assert.strictEqual( dptcon.length, 9, 'has expected arity' );
});

test( 'dptcon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dptcon( -1, 2, 1, 2, 1, 2, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
