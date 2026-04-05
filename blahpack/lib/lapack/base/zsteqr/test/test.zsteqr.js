/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsteqr = require( './../lib/zsteqr.js' );


// TESTS //

test( 'zsteqr is a function', function t() {
	assert.strictEqual( typeof zsteqr, 'function', 'is a function' );
});

test( 'zsteqr has expected arity', function t() {
	assert.strictEqual( zsteqr.length, 11, 'has expected arity' );
});

test( 'zsteqr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zsteqr( 'invalid', 2, new Float64Array( 4 ), 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zsteqr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsteqr( 'row-major', 2, -1, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
