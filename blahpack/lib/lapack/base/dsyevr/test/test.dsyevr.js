/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsyevr = require( './../lib/dsyevr.js' );


// TESTS //

test( 'dsyevr is a function', function t() {
	assert.strictEqual( typeof dsyevr, 'function', 'is a function' );
});

test( 'dsyevr has expected arity', function t() {
	assert.strictEqual( dsyevr.length, 24, 'has expected arity' );
});

test( 'dsyevr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsyevr( 2, 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'dsyevr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsyevr( 2, 2, 'upper', -1, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
