/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsytri = require( './../lib/zsytri.js' );


// TESTS //

test( 'zsytri is a function', function t() {
	assert.strictEqual( typeof zsytri, 'function', 'is a function' );
});

test( 'zsytri has expected arity', function t() {
	assert.strictEqual( zsytri.length, 5, 'has expected arity' );
});

test( 'zsytri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsytri( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zsytri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsytri( 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
