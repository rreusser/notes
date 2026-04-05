/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dorg2l = require( './../lib/dorg2l.js' );


// TESTS //

test( 'dorg2l is a function', function t() {
	assert.strictEqual( typeof dorg2l, 'function', 'is a function' );
});

test( 'dorg2l has expected arity', function t() {
	assert.strictEqual( dorg2l.length, 10, 'has expected arity' );
});

test( 'dorg2l throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dorg2l( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dorg2l throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dorg2l( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorg2l throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dorg2l( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorg2l throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dorg2l( 'row-major', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
