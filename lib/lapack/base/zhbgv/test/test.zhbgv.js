/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhbgv = require( './../lib/zhbgv.js' );


// TESTS //

test( 'zhbgv is a function', function t() {
	assert.strictEqual( typeof zhbgv, 'function', 'is a function' );
});

test( 'zhbgv has expected arity', function t() {
	assert.strictEqual( zhbgv.length, 18, 'has expected arity' );
});

test( 'zhbgv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhbgv( 'invalid', 2, 'upper', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhbgv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhbgv( 'row-major', 2, 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhbgv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhbgv( 'row-major', 2, 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
