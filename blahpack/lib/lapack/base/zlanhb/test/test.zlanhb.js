/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlanhb = require( './../lib/zlanhb.js' );


// TESTS //

test( 'zlanhb is a function', function t() {
	assert.strictEqual( typeof zlanhb, 'function', 'is a function' );
});

test( 'zlanhb has expected arity', function t() {
	assert.strictEqual( zlanhb.length, 7, 'has expected arity' );
});

test( 'zlanhb throws TypeError for invalid norm', function t() {
	assert.throws( function throws() {
		zlanhb( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlanhb throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlanhb( 'max', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlanhb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlanhb( 'max', 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zlanhb throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zlanhb( 'max', 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
