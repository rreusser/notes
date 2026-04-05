/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhpmv = require( './../lib/zhpmv.js' );


// TESTS //

test( 'zhpmv is a function', function t() {
	assert.strictEqual( typeof zhpmv, 'function', 'is a function' );
});

test( 'zhpmv has expected arity', function t() {
	assert.strictEqual( zhpmv.length, 10, 'has expected arity' );
});

test( 'zhpmv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhpmv( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'zhpmv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhpmv( 'upper', -1, 2, new Float64Array( 4 ), 1, 2, 1, 2, 2, 1 );
	}, RangeError );
});
