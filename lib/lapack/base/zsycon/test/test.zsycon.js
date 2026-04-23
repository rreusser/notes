/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsycon = require( './../lib/zsycon.js' );


// TESTS //

test( 'zsycon is a function', function t() {
	assert.strictEqual( typeof zsycon, 'function', 'is a function' );
});

test( 'zsycon has expected arity', function t() {
	assert.strictEqual( zsycon.length, 10, 'has expected arity' );
});

test( 'zsycon throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsycon( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zsycon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsycon( 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
