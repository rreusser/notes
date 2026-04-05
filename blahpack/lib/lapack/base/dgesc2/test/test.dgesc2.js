/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgesc2 = require( './../lib/dgesc2.js' );


// TESTS //

test( 'dgesc2 is a function', function t() {
	assert.strictEqual( typeof dgesc2, 'function', 'is a function' );
});

test( 'dgesc2 has expected arity', function t() {
	assert.strictEqual( dgesc2.length, 10, 'has expected arity' );
});

test( 'dgesc2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgesc2( -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
