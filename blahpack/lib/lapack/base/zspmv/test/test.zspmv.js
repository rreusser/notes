/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zspmv = require( './../lib/zspmv.js' );


// TESTS //

test( 'zspmv is a function', function t() {
	assert.strictEqual( typeof zspmv, 'function', 'is a function' );
});

test( 'zspmv has expected arity', function t() {
	assert.strictEqual( zspmv.length, 10, 'has expected arity' );
});

test( 'zspmv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zspmv( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'zspmv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zspmv( 'upper', -1, 2, new Float64Array( 4 ), 1, 2, 1, 2, 2, 1 );
	}, RangeError );
});
