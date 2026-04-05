/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsysv = require( './../lib/zsysv.js' );


// TESTS //

test( 'zsysv is a function', function t() {
	assert.strictEqual( typeof zsysv, 'function', 'is a function' );
});

test( 'zsysv has expected arity', function t() {
	assert.strictEqual( zsysv.length, 9, 'has expected arity' );
});

test( 'zsysv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsysv( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zsysv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsysv( 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zsysv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zsysv( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
