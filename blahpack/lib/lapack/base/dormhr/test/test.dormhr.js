/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dormhr = require( './../lib/dormhr.js' );


// TESTS //

test( 'dormhr is a function', function t() {
	assert.strictEqual( typeof dormhr, 'function', 'is a function' );
});

test( 'dormhr has expected arity', function t() {
	assert.strictEqual( dormhr.length, 15, 'has expected arity' );
});

test( 'dormhr throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dormhr( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'dormhr throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dormhr( 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'dormhr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dormhr( 'left', 'no-transpose', -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'dormhr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dormhr( 'left', 'no-transpose', new Float64Array( 4 ), -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
