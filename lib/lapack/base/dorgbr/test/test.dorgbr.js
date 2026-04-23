/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dorgbr = require( './../lib/dorgbr.js' );


// TESTS //

test( 'dorgbr is a function', function t() {
	assert.strictEqual( typeof dorgbr, 'function', 'is a function' );
});

test( 'dorgbr has expected arity', function t() {
	assert.strictEqual( dorgbr.length, 11, 'has expected arity' );
});

test( 'dorgbr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dorgbr( 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dorgbr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dorgbr( 'row-major', 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorgbr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dorgbr( 'row-major', 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorgbr throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dorgbr( 'row-major', 2, new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
