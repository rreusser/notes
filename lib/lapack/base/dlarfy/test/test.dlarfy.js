
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarfy = require( './../lib/dlarfy.js' );


// TESTS //

test( 'dlarfy is a function', function t() {
	assert.strictEqual( typeof dlarfy, 'function', 'is a function' );
});

test( 'dlarfy has expected arity', function t() {
	assert.strictEqual( dlarfy.length, 10, 'has expected arity' );
});

test( 'dlarfy throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlarfy( 'invalid', 'upper', 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlarfy throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlarfy( 'row-major', 'invalid', 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlarfy throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarfy( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
