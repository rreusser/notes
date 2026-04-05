/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztpsv = require( './../lib/ztpsv.js' );


// TESTS //

test( 'ztpsv is a function', function t() {
	assert.strictEqual( typeof ztpsv, 'function', 'is a function' );
});

test( 'ztpsv has expected arity', function t() {
	assert.strictEqual( ztpsv.length, 8, 'has expected arity' );
});

test( 'ztpsv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztpsv( 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1 );
	}, TypeError );
});

test( 'ztpsv throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ztpsv( 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1 );
	}, TypeError );
});

test( 'ztpsv throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztpsv( 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1 );
	}, TypeError );
});

test( 'ztpsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztpsv( 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 1, 2, 1 );
	}, RangeError );
});
