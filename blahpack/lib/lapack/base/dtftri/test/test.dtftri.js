/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtftri = require( './../lib/dtftri.js' );


// TESTS //

test( 'dtftri is a function', function t() {
	assert.strictEqual( typeof dtftri, 'function', 'is a function' );
});

test( 'dtftri has expected arity', function t() {
	assert.strictEqual( dtftri.length, 5, 'has expected arity' );
});

test( 'dtftri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtftri( 2, 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dtftri throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtftri( 2, 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dtftri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtftri( 2, 'upper', 'non-unit', -1, new Float64Array( 4 ) );
	}, RangeError );
});
