/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgesvd = require( './../lib/dgesvd.js' );


// TESTS //

test( 'dgesvd is a function', function t() {
	assert.strictEqual( typeof dgesvd, 'function', 'is a function' );
});

test( 'dgesvd has expected arity', function t() {
	assert.strictEqual( dgesvd.length, 13, 'has expected arity' );
});

test( 'dgesvd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgesvd( 'invalid', 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgesvd throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgesvd( 'row-major', 2, 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dgesvd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgesvd( 'row-major', 2, 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
