/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsyconvf = require( './../lib/zsyconvf.js' );


// TESTS //

test( 'zsyconvf is a function', function t() {
	assert.strictEqual( typeof zsyconvf, 'function', 'is a function' );
});

test( 'zsyconvf has expected arity', function t() {
	assert.strictEqual( zsyconvf.length, 11, 'has expected arity' );
});

test( 'zsyconvf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zsyconvf( 'invalid', 'upper', 'no-transpose', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'zsyconvf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsyconvf( 'row-major', 'invalid', 'no-transpose', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'zsyconvf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsyconvf( 'row-major', 'upper', 'no-transpose', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});
