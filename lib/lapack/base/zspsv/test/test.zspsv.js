/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zspsv = require( './../lib/zspsv.js' );


// TESTS //

test( 'zspsv is a function', function t() {
	assert.strictEqual( typeof zspsv, 'function', 'is a function' );
});

test( 'zspsv has expected arity', function t() {
	assert.strictEqual( zspsv.length, 8, 'has expected arity' );
});

test( 'zspsv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zspsv( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zspsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zspsv( 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zspsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zspsv( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
