/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpptrf = require( './../lib/zpptrf.js' );


// TESTS //

test( 'zpptrf is a function', function t() {
	assert.strictEqual( typeof zpptrf, 'function', 'is a function' );
});

test( 'zpptrf has expected arity', function t() {
	assert.strictEqual( zpptrf.length, 3, 'has expected arity' );
});

test( 'zpptrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpptrf( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpptrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpptrf( 'upper', -1, new Float64Array( 4 ) );
	}, RangeError );
});
