/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dspmv = require( './../lib/dspmv.js' );


// TESTS //

test( 'dspmv is a function', function t() {
	assert.strictEqual( typeof dspmv, 'function', 'is a function' );
});

test( 'dspmv has expected arity', function t() {
	assert.strictEqual( dspmv.length, 10, 'has expected arity' );
});

test( 'dspmv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dspmv( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'dspmv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dspmv( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'dspmv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dspmv( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), 2, 1, 2, 2, 1 );
	}, RangeError );
});
