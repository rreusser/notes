/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsytrf = require( './../lib/zsytrf.js' );


// TESTS //

test( 'zsytrf is a function', function t() {
	assert.strictEqual( typeof zsytrf, 'function', 'is a function' );
});

test( 'zsytrf has expected arity', function t() {
	assert.strictEqual( zsytrf.length, 9, 'has expected arity' );
});

test( 'zsytrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsytrf( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 1, 1, 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zsytrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsytrf( 'upper', -1, new Float64Array( 4 ), 1, 1, 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
