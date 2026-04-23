/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_gbrcond_c = require( './../lib/zla_gbrcond_c.js' );


// TESTS //

test( 'zla_gbrcond_c is a function', function t() {
	assert.strictEqual( typeof zla_gbrcond_c, 'function', 'is a function' );
});

test( 'zla_gbrcond_c has expected arity', function t() {
	assert.strictEqual( zla_gbrcond_c.length, 19, 'has expected arity' );
});

test( 'zla_gbrcond_c throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zla_gbrcond_c( 'invalid', 'no-transpose', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zla_gbrcond_c throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zla_gbrcond_c( 'row-major', 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zla_gbrcond_c throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zla_gbrcond_c( 'row-major', 'no-transpose', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
