/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsycon = require( './../lib/dsycon.js' );


// TESTS //

test( 'dsycon is a function', function t() {
	assert.strictEqual( typeof dsycon, 'function', 'is a function' );
});

test( 'dsycon has expected arity', function t() {
	assert.strictEqual( dsycon.length, 12, 'has expected arity' );
});

test( 'dsycon throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsycon( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsycon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsycon( 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
