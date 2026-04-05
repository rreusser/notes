/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpotrs = require( './../lib/zpotrs.js' );


// TESTS //

test( 'zpotrs is a function', function t() {
	assert.strictEqual( typeof zpotrs, 'function', 'is a function' );
});

test( 'zpotrs has expected arity', function t() {
	assert.strictEqual( zpotrs.length, 8, 'has expected arity' );
});

test( 'zpotrs throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zpotrs( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpotrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpotrs( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpotrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpotrs( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zpotrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zpotrs( 'row-major', 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
