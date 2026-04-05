/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dppsv = require( './../lib/dppsv.js' );


// TESTS //

test( 'dppsv is a function', function t() {
	assert.strictEqual( typeof dppsv, 'function', 'is a function' );
});

test( 'dppsv has expected arity', function t() {
	assert.strictEqual( dppsv.length, 6, 'has expected arity' );
});

test( 'dppsv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dppsv( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dppsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dppsv( 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dppsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dppsv( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
