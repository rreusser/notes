/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsymv = require( './../lib/dsymv.js' );


// TESTS //

test( 'dsymv is a function', function t() {
	assert.strictEqual( typeof dsymv, 'function', 'is a function' );
});

test( 'dsymv has expected arity', function t() {
	assert.strictEqual( dsymv.length, 11, 'has expected arity' );
});

test( 'dsymv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsymv( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'dsymv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsymv( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'dsymv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsymv( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, RangeError );
});
