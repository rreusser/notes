/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpteqr = require( './../lib/zpteqr.js' );


// TESTS //

test( 'zpteqr is a function', function t() {
	assert.strictEqual( typeof zpteqr, 'function', 'is a function' );
});

test( 'zpteqr has expected arity', function t() {
	assert.strictEqual( zpteqr.length, 11, 'has expected arity' );
});

test( 'zpteqr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zpteqr( 'invalid', 2, new Float64Array( 4 ), 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zpteqr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpteqr( 'row-major', 2, -1, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
