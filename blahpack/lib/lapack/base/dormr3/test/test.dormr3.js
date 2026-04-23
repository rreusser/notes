/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dormr3 = require( './../lib/dormr3.js' );


// TESTS //

test( 'dormr3 is a function', function t() {
	assert.strictEqual( typeof dormr3, 'function', 'is a function' );
});

test( 'dormr3 has expected arity', function t() {
	assert.strictEqual( dormr3.length, 15, 'has expected arity' );
});

test( 'dormr3 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dormr3( 'invalid', 'left', 'no-transpose', 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dormr3 throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dormr3( 'row-major', 'invalid', 'no-transpose', 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dormr3 throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dormr3( 'row-major', 'left', 'invalid', 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dormr3 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dormr3( 'row-major', 'left', 'no-transpose', -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dormr3 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dormr3( 'row-major', 'left', 'no-transpose', 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dormr3 throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dormr3( 'row-major', 'left', 'no-transpose', 2, 2, -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
