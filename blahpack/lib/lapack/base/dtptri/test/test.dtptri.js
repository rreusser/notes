/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtptri = require( './../lib/dtptri.js' );


// TESTS //

test( 'dtptri is a function', function t() {
	assert.strictEqual( typeof dtptri, 'function', 'is a function' );
});

test( 'dtptri has expected arity', function t() {
	assert.strictEqual( dtptri.length, 4, 'has expected arity' );
});

test( 'dtptri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtptri( 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dtptri throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtptri( 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dtptri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtptri( 'upper', 'non-unit', -1, new Float64Array( 4 ) );
	}, RangeError );
});
