/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsptri = require( './../lib/zsptri.js' );


// TESTS //

test( 'zsptri is a function', function t() {
	assert.strictEqual( typeof zsptri, 'function', 'is a function' );
});

test( 'zsptri has expected arity', function t() {
	assert.strictEqual( zsptri.length, 5, 'has expected arity' );
});

test( 'zsptri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsptri( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zsptri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsptri( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
