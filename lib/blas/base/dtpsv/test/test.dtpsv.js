/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtpsv = require( './../lib/dtpsv.js' );


// TESTS //

test( 'dtpsv is a function', function t() {
	assert.strictEqual( typeof dtpsv, 'function', 'is a function' );
});

test( 'dtpsv has expected arity', function t() {
	assert.strictEqual( dtpsv.length, 8, 'has expected arity' );
});

test( 'dtpsv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtpsv( 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1 );
	}, TypeError );
});

test( 'dtpsv throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dtpsv( 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1 );
	}, TypeError );
});

test( 'dtpsv throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtpsv( 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1 );
	}, TypeError );
});

test( 'dtpsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtpsv( 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 1, 2, 1 );
	}, RangeError );
});
