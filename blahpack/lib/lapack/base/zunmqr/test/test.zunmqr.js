/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zunmqr = require( './../lib/zunmqr.js' );


// TESTS //

test( 'zunmqr is a function', function t() {
	assert.strictEqual( typeof zunmqr, 'function', 'is a function' );
});

test( 'zunmqr has expected arity', function t() {
	assert.strictEqual( zunmqr.length, 14, 'has expected arity' );
});

test( 'zunmqr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zunmqr( 'invalid', 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunmqr throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zunmqr( 'row-major', 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunmqr throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zunmqr( 'row-major', 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunmqr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zunmqr( 'row-major', 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunmqr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zunmqr( 'row-major', 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunmqr throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zunmqr( 'row-major', 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
