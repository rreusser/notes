/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtfttr = require( './../lib/dtfttr.js' );


// TESTS //

test( 'dtfttr is a function', function t() {
	assert.strictEqual( typeof dtfttr, 'function', 'is a function' );
});

test( 'dtfttr has expected arity', function t() {
	assert.strictEqual( dtfttr.length, 7, 'has expected arity' );
});

test( 'dtfttr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtfttr( 'invalid', 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtfttr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtfttr( 'row-major', 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtfttr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtfttr( 'row-major', 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
