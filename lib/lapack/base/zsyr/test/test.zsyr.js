/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsyr = require( './../lib/zsyr.js' );


// TESTS //

test( 'zsyr is a function', function t() {
	assert.strictEqual( typeof zsyr, 'function', 'is a function' );
});

test( 'zsyr has expected arity', function t() {
	assert.strictEqual( zsyr.length, 8, 'has expected arity' );
});

test( 'zsyr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zsyr( 'invalid', 'upper', new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zsyr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsyr( 'row-major', 'invalid', new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zsyr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsyr( 'row-major', 'upper', -1, 2, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
