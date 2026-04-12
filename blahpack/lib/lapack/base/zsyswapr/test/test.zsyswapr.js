/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zsyswapr = require( './../lib/zsyswapr.js' );


// TESTS //

test( 'zsyswapr is a function', function t() {
	assert.strictEqual( typeof zsyswapr, 'function', 'is a function' );
});

test( 'zsyswapr has expected arity', function t() {
	assert.strictEqual( zsyswapr.length, 7, 'has expected arity' );
});

test( 'zsyswapr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zsyswapr( 'invalid', 'upper', 2, new Complex128Array( 4 ), 2, 0, 1 );
	}, TypeError );
});

test( 'zsyswapr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsyswapr( 'row-major', 'invalid', 2, new Complex128Array( 4 ), 2, 0, 1 );
	}, TypeError );
});

test( 'zsyswapr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsyswapr( 'row-major', 'upper', -1, new Complex128Array( 4 ), 2, 0, 1 );
	}, RangeError );
});
