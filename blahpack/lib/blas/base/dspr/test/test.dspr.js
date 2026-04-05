/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dspr = require( './../lib/dspr.js' );


// TESTS //

test( 'dspr is a function', function t() {
	assert.strictEqual( typeof dspr, 'function', 'is a function' );
});

test( 'dspr has expected arity', function t() {
	assert.strictEqual( dspr.length, 7, 'has expected arity' );
});

test( 'dspr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dspr( 'invalid', new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dspr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dspr( 'upper', -1, 2, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
