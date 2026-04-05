/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhpr = require( './../lib/zhpr.js' );


// TESTS //

test( 'zhpr is a function', function t() {
	assert.strictEqual( typeof zhpr, 'function', 'is a function' );
});

test( 'zhpr has expected arity', function t() {
	assert.strictEqual( zhpr.length, 7, 'has expected arity' );
});

test( 'zhpr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhpr( 'invalid', new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhpr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhpr( 'upper', -1, 2, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
