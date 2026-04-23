/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsytrs3 = require( './../lib/dsytrs_3.js' );


// TESTS //

test( 'dsytrs3 is a function', function t() {
	assert.strictEqual( typeof dsytrs3, 'function', 'is a function' );
});

test( 'dsytrs3 has expected arity', function t() {
	assert.strictEqual( dsytrs3.length, 12, 'has expected arity' );
});

test( 'dsytrs3 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsytrs3( 'invalid', 'upper', 2, 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Int32Array( 2 ), 1, new Float64Array( 2 ), 2 );
	}, TypeError );
});

test( 'dsytrs3 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsytrs3( 'row-major', 'invalid', 2, 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Int32Array( 2 ), 1, new Float64Array( 2 ), 2 );
	}, TypeError );
});

test( 'dsytrs3 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsytrs3( 'row-major', 'upper', -1, 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Int32Array( 2 ), 1, new Float64Array( 2 ), 2 );
	}, RangeError );
});

test( 'dsytrs3 throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dsytrs3( 'row-major', 'upper', 2, -1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Int32Array( 2 ), 1, new Float64Array( 2 ), 2 );
	}, RangeError );
});

test( 'dsytrs3 throws RangeError for LDA < N', function t() {
	assert.throws( function throws() {
		dsytrs3( 'row-major', 'upper', 2, 1, new Float64Array( 4 ), 1, new Float64Array( 2 ), 1, new Int32Array( 2 ), 1, new Float64Array( 2 ), 2 );
	}, RangeError );
});

test( 'dsytrs3 throws RangeError for LDB < N', function t() {
	assert.throws( function throws() {
		dsytrs3( 'row-major', 'upper', 2, 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), 1, new Int32Array( 2 ), 1, new Float64Array( 2 ), 1 );
	}, RangeError );
});
