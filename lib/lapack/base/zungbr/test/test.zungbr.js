/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zungbr = require( './../lib/zungbr.js' );


// TESTS //

test( 'zungbr is a function', function t() {
	assert.strictEqual( typeof zungbr, 'function', 'is a function' );
});

test( 'zungbr has expected arity', function t() {
	assert.strictEqual( zungbr.length, 11, 'has expected arity' );
});

test( 'zungbr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zungbr( 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zungbr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zungbr( 'row-major', 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zungbr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zungbr( 'row-major', 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zungbr throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zungbr( 'row-major', 2, new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
