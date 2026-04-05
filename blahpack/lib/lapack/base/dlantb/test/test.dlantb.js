/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlantb = require( './../lib/dlantb.js' );


// TESTS //

test( 'dlantb is a function', function t() {
	assert.strictEqual( typeof dlantb, 'function', 'is a function' );
});

test( 'dlantb has expected arity', function t() {
	assert.strictEqual( dlantb.length, 8, 'has expected arity' );
});

test( 'dlantb throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlantb( 2, 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dlantb throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dlantb( 2, 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dlantb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlantb( 2, 'upper', 'non-unit', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});

test( 'dlantb throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dlantb( 2, 'upper', 'non-unit', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
