/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarfx = require( './../lib/dlarfx.js' );


// TESTS //

test( 'dlarfx is a function', function t() {
	assert.strictEqual( typeof dlarfx, 'function', 'is a function' );
});

test( 'dlarfx has expected arity', function t() {
	assert.strictEqual( dlarfx.length, 10, 'has expected arity' );
});

test( 'dlarfx throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dlarfx( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlarfx throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlarfx( 'left', -1, new Float64Array( 4 ), 2, 1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dlarfx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarfx( 'left', new Float64Array( 4 ), -1, 2, 1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
