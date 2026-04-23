/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dposv = require( './../lib/dposv.js' );


// TESTS //

test( 'dposv is a function', function t() {
	assert.strictEqual( typeof dposv, 'function', 'is a function' );
});

test( 'dposv has expected arity', function t() {
	assert.strictEqual( dposv.length, 8, 'has expected arity' );
});

test( 'dposv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dposv( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dposv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dposv( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dposv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dposv( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dposv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dposv( 'row-major', 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
