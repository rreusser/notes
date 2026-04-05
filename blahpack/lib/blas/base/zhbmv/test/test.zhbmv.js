/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhbmv = require( './../lib/zhbmv.js' );


// TESTS //

test( 'zhbmv is a function', function t() {
	assert.strictEqual( typeof zhbmv, 'function', 'is a function' );
});

test( 'zhbmv has expected arity', function t() {
	assert.strictEqual( zhbmv.length, 12, 'has expected arity' );
});

test( 'zhbmv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhbmv( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'zhbmv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhbmv( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'zhbmv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhbmv( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, RangeError );
});

test( 'zhbmv throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zhbmv( 'row-major', 'upper', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, RangeError );
});
