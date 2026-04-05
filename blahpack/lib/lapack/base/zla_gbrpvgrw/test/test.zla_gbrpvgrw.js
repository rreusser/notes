/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_gbrpvgrw = require( './../lib/zla_gbrpvgrw.js' );


// TESTS //

test( 'zla_gbrpvgrw is a function', function t() {
	assert.strictEqual( typeof zla_gbrpvgrw, 'function', 'is a function' );
});

test( 'zla_gbrpvgrw has expected arity', function t() {
	assert.strictEqual( zla_gbrpvgrw.length, 9, 'has expected arity' );
});

test( 'zla_gbrpvgrw throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zla_gbrpvgrw( 'invalid', new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zla_gbrpvgrw throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zla_gbrpvgrw( 'row-major', -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
