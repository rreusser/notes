/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlantr = require( './../lib/zlantr.js' );


// TESTS //

test( 'zlantr is a function', function t() {
	assert.strictEqual( typeof zlantr, 'function', 'is a function' );
});

test( 'zlantr has expected arity', function t() {
	assert.strictEqual( zlantr.length, 9, 'has expected arity' );
});

test( 'zlantr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlantr( 2, 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlantr throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		zlantr( 2, 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlantr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlantr( 2, 'upper', 'non-unit', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zlantr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlantr( 2, 'upper', 'non-unit', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
