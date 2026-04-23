/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zher2 = require( './../lib/zher2.js' );


// TESTS //

test( 'zher2 is a function', function t() {
	assert.strictEqual( typeof zher2, 'function', 'is a function' );
});

test( 'zher2 has expected arity', function t() {
	assert.strictEqual( zher2.length, 10, 'has expected arity' );
});

test( 'zher2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zher2( 'invalid', 'upper', new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zher2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zher2( 'row-major', 'invalid', new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zher2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zher2( 'row-major', 'upper', -1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
