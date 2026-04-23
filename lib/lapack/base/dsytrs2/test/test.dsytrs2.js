/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsytrs2 = require( './../lib/dsytrs2.js' );


// TESTS //

test( 'dsytrs2 is a function', function t() {
	assert.strictEqual( typeof dsytrs2, 'function', 'is a function' );
});

test( 'dsytrs2 has expected arity', function t() {
	assert.strictEqual( dsytrs2.length, 11, 'has expected arity' );
});

test( 'dsytrs2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsytrs2( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsytrs2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsytrs2( 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dsytrs2 throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dsytrs2( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
