/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlacp2 = require( './../lib/zlacp2.js' );


// TESTS //

test( 'zlacp2 is a function', function t() {
	assert.strictEqual( typeof zlacp2, 'function', 'is a function' );
});

test( 'zlacp2 has expected arity', function t() {
	assert.strictEqual( zlacp2.length, 8, 'has expected arity' );
});

test( 'zlacp2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlacp2( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlacp2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlacp2( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlacp2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlacp2( 'row-major', 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlacp2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlacp2( 'row-major', 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
