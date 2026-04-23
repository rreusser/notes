/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dormbr = require( './../lib/dormbr.js' );


// TESTS //

test( 'dormbr is a function', function t() {
	assert.strictEqual( typeof dormbr, 'function', 'is a function' );
});

test( 'dormbr has expected arity', function t() {
	assert.strictEqual( dormbr.length, 15, 'has expected arity' );
});

test( 'dormbr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dormbr( 'invalid', 2, 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dormbr throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 2, 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dormbr throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 2, 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dormbr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 2, 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dormbr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 2, 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dormbr throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dormbr( 'row-major', 2, 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
