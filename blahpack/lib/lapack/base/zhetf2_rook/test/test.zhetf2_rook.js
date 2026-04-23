/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhetf2Rook = require( './../lib/zhetf2_rook.js' );


// TESTS //

test( 'zhetf2Rook is a function', function t() {
	assert.strictEqual( typeof zhetf2Rook, 'function', 'is a function' );
});

test( 'zhetf2Rook has expected arity', function t() {
	assert.strictEqual( zhetf2Rook.length, 8, 'has expected arity' );
});

test( 'zhetf2Rook throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhetf2Rook( 'invalid', 'upper', 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'zhetf2Rook throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhetf2Rook( 'row-major', 'invalid', 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'zhetf2Rook throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhetf2Rook( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});
