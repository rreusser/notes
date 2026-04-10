/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtprfb = require( './../lib/dtprfb.js' );


// TESTS //

test( 'dtprfb is a function', function t() {
	assert.strictEqual( typeof dtprfb, 'function', 'is a function' );
});

test( 'dtprfb has expected arity', function t() {
	assert.strictEqual( dtprfb.length, 19, 'has expected arity' );
});

test( 'dtprfb throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtprfb( 'invalid', 'left', 'no-transpose', 'no-transpose', 'no-transpose', 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtprfb throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dtprfb( 'row-major', 'invalid', 'no-transpose', 'no-transpose', 'no-transpose', 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtprfb throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dtprfb( 'row-major', 'left', 'invalid', 'no-transpose', 'no-transpose', 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtprfb throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtprfb( 'row-major', 'left', 'no-transpose', 'no-transpose', 'no-transpose', -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dtprfb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtprfb( 'row-major', 'left', 'no-transpose', 'no-transpose', 'no-transpose', 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dtprfb throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dtprfb( 'row-major', 'left', 'no-transpose', 'no-transpose', 'no-transpose', 2, 2, -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
