/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zheevr = require( './../lib/zheevr.js' );


// TESTS //

test( 'zheevr is a function', function t() {
	assert.strictEqual( typeof zheevr, 'function', 'is a function' );
});

test( 'zheevr has expected arity', function t() {
	assert.strictEqual( zheevr.length, 27, 'has expected arity' );
});

test( 'zheevr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zheevr( 2, 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zheevr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zheevr( 2, 2, 'upper', -1, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
