
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsytf2Rook = require( './../lib/dsytf2_rook.js' );


// TESTS //

test( 'dsytf2Rook is a function', function t() {
	assert.strictEqual( typeof dsytf2Rook, 'function', 'is a function' );
});

test( 'dsytf2Rook has expected arity', function t() {
	assert.strictEqual( dsytf2Rook.length, 8, 'has expected arity' );
});

test( 'dsytf2Rook throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsytf2Rook( 'invalid', 'upper', 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dsytf2Rook throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsytf2Rook( 'row-major', 'invalid', 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dsytf2Rook throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsytf2Rook( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});
