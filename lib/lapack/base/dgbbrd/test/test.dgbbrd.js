/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgbbrd = require( './../lib/dgbbrd.js' );


// TESTS //

test( 'dgbbrd is a function', function t() {
	assert.strictEqual( typeof dgbbrd, 'function', 'is a function' );
});

test( 'dgbbrd has expected arity', function t() {
	assert.strictEqual( dgbbrd.length, 21, 'has expected arity' );
});

test( 'dgbbrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgbbrd( 'invalid', 'no-vectors', 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgbbrd throws TypeError for invalid vect', function t() {
	assert.throws( function throws() {
		dgbbrd( 'row-major', 'bogus', 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgbbrd throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgbbrd( 'row-major', 'no-vectors', -1, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dgbbrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgbbrd( 'row-major', 'no-vectors', 2, -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
