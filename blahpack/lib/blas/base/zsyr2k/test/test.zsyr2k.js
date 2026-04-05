/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsyr2k = require( './../lib/zsyr2k.js' );


// TESTS //

test( 'zsyr2k is a function', function t() {
	assert.strictEqual( typeof zsyr2k, 'function', 'is a function' );
});

test( 'zsyr2k has expected arity', function t() {
	assert.strictEqual( zsyr2k.length, 13, 'has expected arity' );
});

test( 'zsyr2k throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zsyr2k( 'invalid', 'upper', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zsyr2k throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsyr2k( 'row-major', 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zsyr2k throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zsyr2k( 'row-major', 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zsyr2k throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsyr2k( 'row-major', 'upper', 'no-transpose', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zsyr2k throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zsyr2k( 'row-major', 'upper', 'no-transpose', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
