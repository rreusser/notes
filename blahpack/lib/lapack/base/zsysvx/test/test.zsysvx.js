/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsysvx = require( './../lib/zsysvx.js' );


// TESTS //

test( 'zsysvx is a function', function t() {
	assert.strictEqual( typeof zsysvx, 'function', 'is a function' );
});

test( 'zsysvx has expected arity', function t() {
	assert.strictEqual( zsysvx.length, 24, 'has expected arity' );
});

test( 'zsysvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsysvx( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zsysvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsysvx( 2, 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zsysvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zsysvx( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
