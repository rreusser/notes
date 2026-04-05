/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlauu2 = require( './../lib/zlauu2.js' );


// TESTS //

test( 'zlauu2 is a function', function t() {
	assert.strictEqual( typeof zlauu2, 'function', 'is a function' );
});

test( 'zlauu2 has expected arity', function t() {
	assert.strictEqual( zlauu2.length, 5, 'has expected arity' );
});

test( 'zlauu2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlauu2( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlauu2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlauu2( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlauu2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlauu2( 'row-major', 'upper', -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
