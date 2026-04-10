

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dhsein = require( './../lib/dhsein.js' );


// TESTS //

test( 'dhsein is a function', function t() {
	assert.strictEqual( typeof dhsein, 'function', 'is a function' );
});

test( 'dhsein has expected arity', function t() {
	assert.strictEqual( dhsein.length, 27, 'has expected arity' );
});

test( 'dhsein throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dhsein( 'invalid', 'left', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dhsein throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dhsein( 'row-major', 'invalid', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dhsein throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dhsein( 'row-major', 'left', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2 );
	}, RangeError );
});

test( 'dhsein throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dhsein( 'row-major', 'left', 'no-transpose', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2 );
	}, RangeError );
});

