/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqsp = require( './../lib/dlaqsp.js' );


// TESTS //

test( 'dlaqsp is a function', function t() {
	assert.strictEqual( typeof dlaqsp, 'function', 'is a function' );
});

test( 'dlaqsp has expected arity', function t() {
	assert.strictEqual( dlaqsp.length, 7, 'has expected arity' );
});

test( 'dlaqsp throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlaqsp( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 2 );
	}, TypeError );
});

test( 'dlaqsp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaqsp( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 2 );
	}, RangeError );
});
