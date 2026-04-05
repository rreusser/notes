/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhpr2 = require( './../lib/zhpr2.js' );


// TESTS //

test( 'zhpr2 is a function', function t() {
	assert.strictEqual( typeof zhpr2, 'function', 'is a function' );
});

test( 'zhpr2 has expected arity', function t() {
	assert.strictEqual( zhpr2.length, 9, 'has expected arity' );
});

test( 'zhpr2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhpr2( 'invalid', new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhpr2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhpr2( 'upper', -1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
