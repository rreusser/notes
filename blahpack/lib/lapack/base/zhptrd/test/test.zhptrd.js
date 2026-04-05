/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhptrd = require( './../lib/zhptrd.js' );


// TESTS //

test( 'zhptrd is a function', function t() {
	assert.strictEqual( typeof zhptrd, 'function', 'is a function' );
});

test( 'zhptrd has expected arity', function t() {
	assert.strictEqual( zhptrd.length, 6, 'has expected arity' );
});

test( 'zhptrd throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhptrd( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhptrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhptrd( 'upper', -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ) );
	}, RangeError );
});
