/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsyconv = require( './../lib/dsyconv.js' );


// TESTS //

test( 'dsyconv is a function', function t() {
	assert.strictEqual( typeof dsyconv, 'function', 'is a function' );
});

test( 'dsyconv has expected arity', function t() {
	assert.strictEqual( dsyconv.length, 9, 'has expected arity' );
});

test( 'dsyconv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsyconv( 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsyconv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsyconv( 'upper', 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
