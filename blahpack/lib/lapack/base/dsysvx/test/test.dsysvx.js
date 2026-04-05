/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsysvx = require( './../lib/dsysvx.js' );


// TESTS //

test( 'dsysvx is a function', function t() {
	assert.strictEqual( typeof dsysvx, 'function', 'is a function' );
});

test( 'dsysvx has expected arity', function t() {
	assert.strictEqual( dsysvx.length, 24, 'has expected arity' );
});

test( 'dsysvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsysvx( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsysvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsysvx( 2, 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dsysvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dsysvx( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
