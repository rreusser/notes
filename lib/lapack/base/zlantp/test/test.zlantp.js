/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlantp = require( './../lib/zlantp.js' );


// TESTS //

test( 'zlantp is a function', function t() {
	assert.strictEqual( typeof zlantp, 'function', 'is a function' );
});

test( 'zlantp has expected arity', function t() {
	assert.strictEqual( zlantp.length, 6, 'has expected arity' );
});

test( 'zlantp throws TypeError for invalid norm', function t() {
	assert.throws( function throws() {
		zlantp( 'invalid', 'upper', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlantp throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlantp( 'max', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlantp throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		zlantp( 'max', 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlantp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlantp( 'max', 'upper', 'non-unit', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
