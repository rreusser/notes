/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqsb = require( './../lib/dlaqsb.js' );


// TESTS //

test( 'dlaqsb is a function', function t() {
	assert.strictEqual( typeof dlaqsb, 'function', 'is a function' );
});

test( 'dlaqsb has expected arity', function t() {
	assert.strictEqual( dlaqsb.length, 9, 'has expected arity' );
});

test( 'dlaqsb throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlaqsb( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2 );
	}, TypeError );
});

test( 'dlaqsb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaqsb( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2 );
	}, RangeError );
});
