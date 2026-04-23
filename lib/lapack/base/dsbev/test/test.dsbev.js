/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsbev = require( './../lib/dsbev.js' );


// TESTS //

test( 'dsbev is a function', function t() {
	assert.strictEqual( typeof dsbev, 'function', 'is a function' );
});

test( 'dsbev has expected arity', function t() {
	assert.strictEqual( dsbev.length, 13, 'has expected arity' );
});

test( 'dsbev throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsbev( 'invalid', 2, 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsbev throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsbev( 'row-major', 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsbev throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsbev( 'row-major', 2, 'upper', -1, 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
