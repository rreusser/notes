/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsyev = require( './../lib/dsyev.js' );


// TESTS //

test( 'dsyev is a function', function t() {
	assert.strictEqual( typeof dsyev, 'function', 'is a function' );
});

test( 'dsyev has expected arity', function t() {
	assert.strictEqual( dsyev.length, 10, 'has expected arity' );
});

test( 'dsyev throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsyev( 'invalid', 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsyev throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsyev( 'row-major', 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsyev throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsyev( 'row-major', 2, 'upper', -1, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
