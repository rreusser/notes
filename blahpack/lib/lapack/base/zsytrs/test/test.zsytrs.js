/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsytrs = require( './../lib/zsytrs.js' );


// TESTS //

test( 'zsytrs is a function', function t() {
	assert.strictEqual( typeof zsytrs, 'function', 'is a function' );
});

test( 'zsytrs has expected arity', function t() {
	assert.strictEqual( zsytrs.length, 14, 'has expected arity' );
});

test( 'zsytrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsytrs( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 1, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 1, 2 );
	}, TypeError );
});

test( 'zsytrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsytrs( 'upper', -1, 2, new Float64Array( 4 ), 1, 1, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 1, 2 );
	}, RangeError );
});

test( 'zsytrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zsytrs( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 1, 1, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 1, 2 );
	}, RangeError );
});
