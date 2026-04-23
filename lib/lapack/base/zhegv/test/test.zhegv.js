/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhegv = require( './../lib/zhegv.js' );


// TESTS //

test( 'zhegv is a function', function t() {
	assert.strictEqual( typeof zhegv, 'function', 'is a function' );
});

test( 'zhegv has expected arity', function t() {
	assert.strictEqual( zhegv.length, 15, 'has expected arity' );
});

test( 'zhegv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhegv( 2, 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhegv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhegv( 2, 2, 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
