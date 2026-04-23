/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtpttr = require( './../lib/dtpttr.js' );


// TESTS //

test( 'dtpttr is a function', function t() {
	assert.strictEqual( typeof dtpttr, 'function', 'is a function' );
});

test( 'dtpttr has expected arity', function t() {
	assert.strictEqual( dtpttr.length, 6, 'has expected arity' );
});

test( 'dtpttr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtpttr( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtpttr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtpttr( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtpttr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtpttr( 'row-major', 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
