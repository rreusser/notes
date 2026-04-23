/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zppsv = require( './../lib/zppsv.js' );


// TESTS //

test( 'zppsv is a function', function t() {
	assert.strictEqual( typeof zppsv, 'function', 'is a function' );
});

test( 'zppsv has expected arity', function t() {
	assert.strictEqual( zppsv.length, 7, 'has expected arity' );
});

test( 'zppsv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zppsv( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zppsv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zppsv( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zppsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zppsv( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zppsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zppsv( 'row-major', 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
