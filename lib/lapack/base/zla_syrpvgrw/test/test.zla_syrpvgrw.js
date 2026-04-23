/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_syrpvgrw = require( './../lib/zla_syrpvgrw.js' );


// TESTS //

test( 'zla_syrpvgrw is a function', function t() {
	assert.strictEqual( typeof zla_syrpvgrw, 'function', 'is a function' );
});

test( 'zla_syrpvgrw has expected arity', function t() {
	assert.strictEqual( zla_syrpvgrw.length, 9, 'has expected arity' );
});

test( 'zla_syrpvgrw throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zla_syrpvgrw( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zla_syrpvgrw throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zla_syrpvgrw( 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
