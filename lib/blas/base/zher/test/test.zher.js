/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zher = require( './../lib/zher.js' );


// TESTS //

test( 'zher is a function', function t() {
	assert.strictEqual( typeof zher, 'function', 'is a function' );
});

test( 'zher has expected arity', function t() {
	assert.strictEqual( zher.length, 8, 'has expected arity' );
});

test( 'zher throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zher( 'invalid', 'upper', new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zher throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zher( 'row-major', 'invalid', new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zher throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zher( 'row-major', 'upper', -1, 2, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
