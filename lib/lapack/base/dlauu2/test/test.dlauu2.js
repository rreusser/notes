/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlauu2 = require( './../lib/dlauu2.js' );


// TESTS //

test( 'dlauu2 is a function', function t() {
	assert.strictEqual( typeof dlauu2, 'function', 'is a function' );
});

test( 'dlauu2 has expected arity', function t() {
	assert.strictEqual( dlauu2.length, 5, 'has expected arity' );
});

test( 'dlauu2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlauu2( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlauu2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlauu2( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlauu2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlauu2( 'row-major', 'upper', -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
