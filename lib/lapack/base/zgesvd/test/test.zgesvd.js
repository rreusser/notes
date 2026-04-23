/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgesvd = require( './../lib/zgesvd.js' );


// TESTS //

test( 'zgesvd is a function', function t() {
	assert.strictEqual( typeof zgesvd, 'function', 'is a function' );
});

test( 'zgesvd has expected arity', function t() {
	assert.strictEqual( zgesvd.length, 18, 'has expected arity' );
});

test( 'zgesvd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgesvd( 'invalid', 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgesvd throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgesvd( 'row-major', 2, 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgesvd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgesvd( 'row-major', 2, 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
