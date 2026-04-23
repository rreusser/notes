/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dorm2r = require( './../lib/dorm2r.js' );


// TESTS //

test( 'dorm2r is a function', function t() {
	assert.strictEqual( typeof dorm2r, 'function', 'is a function' );
});

test( 'dorm2r has expected arity', function t() {
	assert.strictEqual( dorm2r.length, 14, 'has expected arity' );
});

test( 'dorm2r throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dorm2r( 'invalid', 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dorm2r throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dorm2r( 'row-major', 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dorm2r throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dorm2r( 'row-major', 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dorm2r throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dorm2r( 'row-major', 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorm2r throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dorm2r( 'row-major', 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorm2r throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dorm2r( 'row-major', 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
