/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zhetri3 = require( './../lib/zhetri_3.js' );


// TESTS //

test( 'zhetri3 is a function', function t() {
	assert.strictEqual( typeof zhetri3, 'function', 'is a function' );
});

test( 'zhetri3 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhetri3( 'invalid', 'lower', 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Int32Array( 1 ), 1, 0, new Complex128Array( 12 ), 1, 12 );
	}, TypeError );
});

test( 'zhetri3 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhetri3( 'column-major', 'invalid', 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Int32Array( 1 ), 1, 0, new Complex128Array( 12 ), 1, 12 );
	}, TypeError );
});

test( 'zhetri3 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhetri3( 'column-major', 'lower', -1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Int32Array( 1 ), 1, 0, new Complex128Array( 12 ), 1, 12 );
	}, RangeError );
});
