/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtbcon = require( './../lib/dtbcon.js' );


// TESTS //

test( 'dtbcon is a function', function t() {
	assert.strictEqual( typeof dtbcon, 'function', 'is a function' );
});

test( 'dtbcon has expected arity', function t() {
	assert.strictEqual( dtbcon.length, 10, 'has expected arity' );
});

test( 'dtbcon throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtbcon( 2, 'invalid', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dtbcon throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtbcon( 2, 'upper', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dtbcon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtbcon( 2, 'upper', 'non-unit', -1, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
