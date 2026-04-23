/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_hercond_x = require( './../lib/zla_hercond_x.js' );


// TESTS //

test( 'zla_hercond_x is a function', function t() {
	assert.strictEqual( typeof zla_hercond_x, 'function', 'is a function' );
});

test( 'zla_hercond_x has expected arity', function t() {
	assert.strictEqual( zla_hercond_x.length, 16, 'has expected arity' );
});

test( 'zla_hercond_x throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zla_hercond_x( 'invalid', 'upper', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zla_hercond_x throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zla_hercond_x( 'row-major', 'invalid', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zla_hercond_x throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zla_hercond_x( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
