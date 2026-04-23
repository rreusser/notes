/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dspgv = require( './../lib/dspgv.js' );


// TESTS //

test( 'dspgv is a function', function t() {
	assert.strictEqual( typeof dspgv, 'function', 'is a function' );
});

test( 'dspgv has expected arity', function t() {
	assert.strictEqual( dspgv.length, 11, 'has expected arity' );
});

test( 'dspgv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dspgv( 'invalid', 2, 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dspgv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dspgv( 'row-major', 2, 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dspgv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dspgv( 'row-major', 2, 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
