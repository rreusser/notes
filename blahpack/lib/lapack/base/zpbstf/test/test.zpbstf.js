/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpbstf = require( './../lib/zpbstf.js' );


// TESTS //

test( 'zpbstf is a function', function t() {
	assert.strictEqual( typeof zpbstf, 'function', 'is a function' );
});

test( 'zpbstf has expected arity', function t() {
	assert.strictEqual( zpbstf.length, 6, 'has expected arity' );
});

test( 'zpbstf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zpbstf( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpbstf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpbstf( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpbstf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpbstf( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
