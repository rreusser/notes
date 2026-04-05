/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlansf = require( './../lib/dlansf.js' );


// TESTS //

test( 'dlansf is a function', function t() {
	assert.strictEqual( typeof dlansf, 'function', 'is a function' );
});

test( 'dlansf has expected arity', function t() {
	assert.strictEqual( dlansf.length, 6, 'has expected arity' );
});

test( 'dlansf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlansf( 2, 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dlansf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlansf( 2, 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
