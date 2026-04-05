/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlarfx = require( './../lib/zlarfx.js' );


// TESTS //

test( 'zlarfx is a function', function t() {
	assert.strictEqual( typeof zlarfx, 'function', 'is a function' );
});

test( 'zlarfx has expected arity', function t() {
	assert.strictEqual( zlarfx.length, 10, 'has expected arity' );
});

test( 'zlarfx throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zlarfx( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlarfx throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlarfx( 'left', -1, new Float64Array( 4 ), 2, 1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zlarfx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarfx( 'left', new Float64Array( 4 ), -1, 2, 1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
