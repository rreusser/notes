/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztpmv = require( './../lib/ztpmv.js' );


// TESTS //

test( 'ztpmv is a function', function t() {
	assert.strictEqual( typeof ztpmv, 'function', 'is a function' );
});

test( 'ztpmv has expected arity', function t() {
	assert.strictEqual( ztpmv.length, 8, 'has expected arity' );
});

test( 'ztpmv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztpmv( 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1 );
	}, TypeError );
});

test( 'ztpmv throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ztpmv( 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1 );
	}, TypeError );
});

test( 'ztpmv throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztpmv( 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1 );
	}, TypeError );
});

test( 'ztpmv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztpmv( 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 1, 2, 1 );
	}, RangeError );
});
