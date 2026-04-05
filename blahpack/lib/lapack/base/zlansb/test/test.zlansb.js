/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansb = require( './../lib/zlansb.js' );


// TESTS //

test( 'zlansb is a function', function t() {
	assert.strictEqual( typeof zlansb, 'function', 'is a function' );
});

test( 'zlansb has expected arity', function t() {
	assert.strictEqual( zlansb.length, 7, 'has expected arity' );
});

test( 'zlansb throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlansb( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlansb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlansb( 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zlansb throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zlansb( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
