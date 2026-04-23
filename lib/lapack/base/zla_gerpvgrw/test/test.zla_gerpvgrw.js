/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_gerpvgrw = require( './../lib/zla_gerpvgrw.js' );


// TESTS //

test( 'zla_gerpvgrw is a function', function t() {
	assert.strictEqual( typeof zla_gerpvgrw, 'function', 'is a function' );
});

test( 'zla_gerpvgrw has expected arity', function t() {
	assert.strictEqual( zla_gerpvgrw.length, 6, 'has expected arity' );
});

test( 'zla_gerpvgrw throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zla_gerpvgrw( -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
