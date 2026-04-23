/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsygst = require( './../lib/dsygst.js' );


// TESTS //

test( 'dsygst is a function', function t() {
	assert.strictEqual( typeof dsygst, 'function', 'is a function' );
});

test( 'dsygst has expected arity', function t() {
	assert.strictEqual( dsygst.length, 7, 'has expected arity' );
});

test( 'dsygst throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsygst( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dsygst throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsygst( 2, 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
