/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsytrs2 = require( './../lib/zsytrs2.js' );


// TESTS //

test( 'zsytrs2 is a function', function t() {
	assert.strictEqual( typeof zsytrs2, 'function', 'is a function' );
});

test( 'zsytrs2 has expected arity', function t() {
	assert.strictEqual( zsytrs2.length, 11, 'has expected arity' );
});

test( 'zsytrs2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsytrs2( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zsytrs2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsytrs2( 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zsytrs2 throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zsytrs2( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
