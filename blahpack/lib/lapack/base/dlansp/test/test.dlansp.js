/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlansp = require( './../lib/dlansp.js' );


// TESTS //

test( 'dlansp is a function', function t() {
	assert.strictEqual( typeof dlansp, 'function', 'is a function' );
});

test( 'dlansp has expected arity', function t() {
	assert.strictEqual( dlansp.length, 5, 'has expected arity' );
});

test( 'dlansp throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlansp( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dlansp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlansp( 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
