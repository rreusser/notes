/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zsytri3 = require( './../lib/zsytri_3.js' );


// TESTS //

test( 'zsytri3 is a function', function t() {
	assert.strictEqual( typeof zsytri3, 'function', 'is a function' );
});

test( 'zsytri3 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zsytri3( 'invalid', 'lower', 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Int32Array( 1 ), 1, 0, new Complex128Array( 12 ), 1, 12 );
	}, TypeError );
});

test( 'zsytri3 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsytri3( 'column-major', 'invalid', 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Int32Array( 1 ), 1, 0, new Complex128Array( 12 ), 1, 12 );
	}, TypeError );
});

test( 'zsytri3 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsytri3( 'column-major', 'lower', -1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Int32Array( 1 ), 1, 0, new Complex128Array( 12 ), 1, 12 );
	}, RangeError );
});
