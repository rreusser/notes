/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zheev = require( './../lib/zheev.js' );


// TESTS //

test( 'zheev is a function', function t() {
	assert.strictEqual( typeof zheev, 'function', 'is a function' );
});

test( 'zheev has expected arity', function t() {
	assert.strictEqual( zheev.length, 13, 'has expected arity' );
});

test( 'zheev throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zheev( 'invalid', 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zheev throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zheev( 'row-major', 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zheev throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zheev( 'row-major', 2, 'upper', -1, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
