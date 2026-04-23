/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlaswp = require( './../lib/dlaswp.js' );


// TESTS //

test( 'dlaswp is a function', function t() {
	assert.strictEqual( typeof dlaswp, 'function', 'is a function' );
});

test( 'dlaswp has expected arity', function t() {
	assert.strictEqual( dlaswp.length, 8, 'has expected arity' );
});

test( 'dlaswp throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlaswp( 'invalid', 2, new Float64Array( 4 ), 2, 0, 1, new Int32Array( [ 1, 0 ] ), 1 );
	}, TypeError );
});

test( 'dlaswp is a no-op when incx is zero', function t() {
	var ipiv = new Int32Array( [ 1, 0 ] );
	var a = new Float64Array( [ 1, 2, 3, 4 ] );
	var out = dlaswp( 'row-major', 2, a, 2, 0, 1, ipiv, 0 );
	assert.deepStrictEqual( Array.from( out ), [ 1, 2, 3, 4 ] );
});
