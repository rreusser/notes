/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhetd2 = require( './../lib/zhetd2.js' );


// TESTS //

test( 'zhetd2 is a function', function t() {
	assert.strictEqual( typeof zhetd2, 'function', 'is a function' );
});

test( 'zhetd2 has expected arity', function t() {
	assert.strictEqual( zhetd2.length, 11, 'has expected arity' );
});

test( 'zhetd2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhetd2( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhetd2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhetd2( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhetd2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhetd2( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
