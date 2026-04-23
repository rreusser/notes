/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpstf2 = require( './../lib/zpstf2.js' );


// TESTS //

test( 'zpstf2 is a function', function t() {
	assert.strictEqual( typeof zpstf2, 'function', 'is a function' );
});

test( 'zpstf2 has expected arity', function t() {
	assert.strictEqual( zpstf2.length, 9, 'has expected arity' );
});

test( 'zpstf2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zpstf2( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpstf2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpstf2( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpstf2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpstf2( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
