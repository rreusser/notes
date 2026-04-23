/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtpttf = require( './../lib/dtpttf.js' );


// TESTS //

test( 'dtpttf is a function', function t() {
	assert.strictEqual( typeof dtpttf, 'function', 'is a function' );
});

test( 'dtpttf has expected arity', function t() {
	assert.strictEqual( dtpttf.length, 5, 'has expected arity' );
});

test( 'dtpttf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtpttf( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dtpttf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtpttf( 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
