/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsytf2rk = require( './../lib/dsytf2_rk.js' );


// TESTS //

test( 'dsytf2rk is a function', function t() {
	assert.strictEqual( typeof dsytf2rk, 'function', 'is a function' );
});

test( 'dsytf2rk has expected arity', function t() {
	assert.strictEqual( dsytf2rk.length, 10, 'has expected arity' );
});

test( 'dsytf2rk throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsytf2rk( 'invalid', 'upper', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dsytf2rk throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsytf2rk( 'row-major', 'invalid', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dsytf2rk throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsytf2rk( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});
