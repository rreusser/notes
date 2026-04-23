/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpptri = require( './../lib/zpptri.js' );


// TESTS //

test( 'zpptri is a function', function t() {
	assert.strictEqual( typeof zpptri, 'function', 'is a function' );
});

test( 'zpptri has expected arity', function t() {
	assert.strictEqual( zpptri.length, 3, 'has expected arity' );
});

test( 'zpptri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpptri( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpptri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpptri( 'upper', -1, new Float64Array( 4 ) );
	}, RangeError );
});
