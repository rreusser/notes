/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zlarfy = require( './../lib/zlarfy.js' );


// TESTS //

test( 'zlarfy is a function', function t() {
	assert.strictEqual( typeof zlarfy, 'function', 'is a function' );
});

test( 'zlarfy has expected arity', function t() {
	assert.strictEqual( zlarfy.length, 10, 'has expected arity' );
});

test( 'zlarfy throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlarfy( 'invalid', 'upper', 2, new Complex128Array( 4 ), 1, new Complex128( 1.0, 0.0 ), new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlarfy throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlarfy( 'row-major', 'invalid', 2, new Complex128Array( 4 ), 1, new Complex128( 1.0, 0.0 ), new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlarfy throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarfy( 'row-major', 'upper', -1, new Complex128Array( 4 ), 1, new Complex128( 1.0, 0.0 ), new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 1 );
	}, RangeError );
});
