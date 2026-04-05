/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrmv = require( './../lib/dtrmv.js' );


// TESTS //

test( 'dtrmv is a function', function t() {
	assert.strictEqual( typeof dtrmv, 'function', 'is a function' );
});

test( 'dtrmv has expected arity', function t() {
	assert.strictEqual( dtrmv.length, 9, 'has expected arity' );
});

test( 'dtrmv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtrmv( 'invalid', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, TypeError );
});

test( 'dtrmv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtrmv( 'row-major', 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, TypeError );
});

test( 'dtrmv throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dtrmv( 'row-major', 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, TypeError );
});

test( 'dtrmv throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtrmv( 'row-major', 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, TypeError );
});

test( 'dtrmv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtrmv( 'row-major', 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});
