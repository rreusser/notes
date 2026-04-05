/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlatbs = require( './../lib/dlatbs.js' );


// TESTS //

test( 'dlatbs is a function', function t() {
	assert.strictEqual( typeof dlatbs, 'function', 'is a function' );
});

test( 'dlatbs has expected arity', function t() {
	assert.strictEqual( dlatbs.length, 13, 'has expected arity' );
});

test( 'dlatbs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlatbs( 'invalid', 'no-transpose', 'non-unit', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlatbs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dlatbs( 'upper', 'invalid', 'non-unit', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlatbs throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dlatbs( 'upper', 'no-transpose', 'invalid', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlatbs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlatbs( 'upper', 'no-transpose', 'non-unit', 2, -1, 2, new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
