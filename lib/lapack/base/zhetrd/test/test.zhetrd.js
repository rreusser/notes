/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhetrd = require( './../lib/zhetrd.js' );


// TESTS //

test( 'zhetrd is a function', function t() {
	assert.strictEqual( typeof zhetrd, 'function', 'is a function' );
});

test( 'zhetrd has expected arity', function t() {
	assert.strictEqual( zhetrd.length, 11, 'has expected arity' );
});

test( 'zhetrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhetrd( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhetrd throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhetrd( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhetrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhetrd( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
