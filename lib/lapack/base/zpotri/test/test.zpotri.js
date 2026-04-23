/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpotri = require( './../lib/zpotri.js' );


// TESTS //

test( 'zpotri is a function', function t() {
	assert.strictEqual( typeof zpotri, 'function', 'is a function' );
});

test( 'zpotri has expected arity', function t() {
	assert.strictEqual( zpotri.length, 5, 'has expected arity' );
});

test( 'zpotri throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zpotri( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpotri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpotri( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpotri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpotri( 'row-major', 'upper', -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
