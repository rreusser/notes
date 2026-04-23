/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zsytri = require( './../lib/zsytri.js' );


// TESTS //

test( 'zsytri is a function', function t() {
	assert.strictEqual( typeof zsytri, 'function', 'is a function' );
});

test( 'zsytri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsytri( 'invalid', 2, new Float64Array( 8 ), 2, new Int32Array( 2 ) );
	}, TypeError );
});

test( 'zsytri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsytri( 'upper', -1, new Float64Array( 4 ), 2, new Int32Array( 2 ) );
	}, RangeError );
});
