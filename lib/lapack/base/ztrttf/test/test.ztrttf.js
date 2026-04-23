/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrttf = require( './../lib/ztrttf.js' );


// TESTS //

test( 'ztrttf is a function', function t() {
	assert.strictEqual( typeof ztrttf, 'function', 'is a function' );
});

test( 'ztrttf has expected arity', function t() {
	assert.strictEqual( ztrttf.length, 7, 'has expected arity' );
});

test( 'ztrttf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztrttf( 'invalid', 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztrttf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztrttf( 'row-major', 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztrttf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztrttf( 'row-major', 2, 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
