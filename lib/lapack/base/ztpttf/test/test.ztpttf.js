/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztpttf = require( './../lib/ztpttf.js' );


// TESTS //

test( 'ztpttf is a function', function t() {
	assert.strictEqual( typeof ztpttf, 'function', 'is a function' );
});

test( 'ztpttf has expected arity', function t() {
	assert.strictEqual( ztpttf.length, 5, 'has expected arity' );
});

test( 'ztpttf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztpttf( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztpttf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztpttf( 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
