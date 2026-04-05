/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgelqf = require( './../lib/zgelqf.js' );


// TESTS //

test( 'zgelqf is a function', function t() {
	assert.strictEqual( typeof zgelqf, 'function', 'is a function' );
});

test( 'zgelqf has expected arity', function t() {
	assert.strictEqual( zgelqf.length, 9, 'has expected arity' );
});

test( 'zgelqf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgelqf( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgelqf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgelqf( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgelqf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgelqf( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
