/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqhb = require( './../lib/zlaqhb.js' );


// TESTS //

test( 'zlaqhb is a function', function t() {
	assert.strictEqual( typeof zlaqhb, 'function', 'is a function' );
});

test( 'zlaqhb has expected arity', function t() {
	assert.strictEqual( zlaqhb.length, 9, 'has expected arity' );
});

test( 'zlaqhb throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlaqhb( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2 );
	}, TypeError );
});

test( 'zlaqhb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaqhb( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2 );
	}, RangeError );
});
