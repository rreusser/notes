/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsytf2rk = require( './../lib/zsytf2_rk.js' );


// TESTS //

test( 'zsytf2_rk is a function', function t() {
	assert.strictEqual( typeof zsytf2rk, 'function', 'is a function' );
});

test( 'zsytf2_rk has expected arity', function t() {
	assert.strictEqual( zsytf2rk.length, 10, 'has expected arity' );
});

test( 'zsytf2_rk throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zsytf2rk( 'invalid', 'upper', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'zsytf2_rk throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsytf2rk( 'row-major', 'invalid', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'zsytf2_rk throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsytf2rk( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});
