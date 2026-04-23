/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsytd2 = require( './../lib/dsytd2.js' );


// TESTS //

test( 'dsytd2 is a function', function t() {
	assert.strictEqual( typeof dsytd2, 'function', 'is a function' );
});

test( 'dsytd2 has expected arity', function t() {
	assert.strictEqual( dsytd2.length, 11, 'has expected arity' );
});

test( 'dsytd2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsytd2( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsytd2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsytd2( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsytd2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsytd2( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
