/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsytf2 = require( './../lib/dsytf2.js' );


// TESTS //

test( 'dsytf2 is a function', function t() {
	assert.strictEqual( typeof dsytf2, 'function', 'is a function' );
});

test( 'dsytf2 has expected arity', function t() {
	assert.strictEqual( dsytf2.length, 7, 'has expected arity' );
});

test( 'dsytf2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsytf2( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsytf2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsytf2( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsytf2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsytf2( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
