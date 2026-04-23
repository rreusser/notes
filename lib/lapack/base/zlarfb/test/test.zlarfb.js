/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlarfb = require( './../lib/zlarfb.js' );


// TESTS //

test( 'zlarfb is a function', function t() {
	assert.strictEqual( typeof zlarfb, 'function', 'is a function' );
});

test( 'zlarfb has expected arity', function t() {
	assert.strictEqual( zlarfb.length, 16, 'has expected arity' );
});

test( 'zlarfb throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlarfb( 'invalid', 'left', 'no-transpose', 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlarfb throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zlarfb( 'row-major', 'invalid', 'no-transpose', 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlarfb throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zlarfb( 'row-major', 'left', 'invalid', 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlarfb throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlarfb( 'row-major', 'left', 'no-transpose', 2, 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlarfb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarfb( 'row-major', 'left', 'no-transpose', 2, 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlarfb throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zlarfb( 'row-major', 'left', 'no-transpose', 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
