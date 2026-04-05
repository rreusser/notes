/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_herpvgrw = require( './../lib/zla_herpvgrw.js' );


// TESTS //

test( 'zla_herpvgrw is a function', function t() {
	assert.strictEqual( typeof zla_herpvgrw, 'function', 'is a function' );
});

test( 'zla_herpvgrw has expected arity', function t() {
	assert.strictEqual( zla_herpvgrw.length, 9, 'has expected arity' );
});

test( 'zla_herpvgrw throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zla_herpvgrw( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zla_herpvgrw throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zla_herpvgrw( 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
