/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zspr = require( './../lib/zspr.js' );


// TESTS //

test( 'zspr is a function', function t() {
	assert.strictEqual( typeof zspr, 'function', 'is a function' );
});

test( 'zspr has expected arity', function t() {
	assert.strictEqual( zspr.length, 7, 'has expected arity' );
});

test( 'zspr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zspr( 'invalid', new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zspr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zspr( 'upper', -1, 2, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
