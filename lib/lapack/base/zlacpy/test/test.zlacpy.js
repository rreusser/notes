/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlacpy = require( './../lib/zlacpy.js' );


// TESTS //

test( 'zlacpy is a function', function t() {
	assert.strictEqual( typeof zlacpy, 'function', 'is a function' );
});

test( 'zlacpy has expected arity', function t() {
	assert.strictEqual( zlacpy.length, 8, 'has expected arity' );
});

test( 'zlacpy throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlacpy( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlacpy throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlacpy( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlacpy throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlacpy( 'row-major', 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlacpy throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlacpy( 'row-major', 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
