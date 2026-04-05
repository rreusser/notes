/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztbcon = require( './../lib/ztbcon.js' );


// TESTS //

test( 'ztbcon is a function', function t() {
	assert.strictEqual( typeof ztbcon, 'function', 'is a function' );
});

test( 'ztbcon has expected arity', function t() {
	assert.strictEqual( ztbcon.length, 12, 'has expected arity' );
});

test( 'ztbcon throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztbcon( 2, 'invalid', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztbcon throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztbcon( 2, 'upper', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztbcon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztbcon( 2, 'upper', 'non-unit', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
