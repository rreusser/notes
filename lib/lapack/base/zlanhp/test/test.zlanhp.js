/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlanhp = require( './../lib/zlanhp.js' );


// TESTS //

test( 'zlanhp is a function', function t() {
	assert.strictEqual( typeof zlanhp, 'function', 'is a function' );
});

test( 'zlanhp has expected arity', function t() {
	assert.strictEqual( zlanhp.length, 5, 'has expected arity' );
});

test( 'zlanhp throws TypeError for invalid norm', function t() {
	assert.throws( function throws() {
		zlanhp( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlanhp throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlanhp( 'max', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlanhp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlanhp( 'max', 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
