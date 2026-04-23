/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhbtrd = require( './../lib/zhbtrd.js' );


// TESTS //

test( 'zhbtrd is a function', function t() {
	assert.strictEqual( typeof zhbtrd, 'function', 'is a function' );
});

test( 'zhbtrd has expected arity', function t() {
	assert.strictEqual( zhbtrd.length, 12, 'has expected arity' );
});

test( 'zhbtrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhbtrd( 'invalid', 2, 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhbtrd throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhbtrd( 'row-major', 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhbtrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhbtrd( 'row-major', 2, 'upper', -1, 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
