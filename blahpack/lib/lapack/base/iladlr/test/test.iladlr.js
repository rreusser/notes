/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var iladlr = require( './../lib/iladlr.js' );


// TESTS //

test( 'iladlr is a function', function t() {
	assert.strictEqual( typeof iladlr, 'function', 'is a function' );
});

test( 'iladlr has expected arity', function t() {
	assert.strictEqual( iladlr.length, 5, 'has expected arity' );
});

test( 'iladlr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		iladlr( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'iladlr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		iladlr( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'iladlr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		iladlr( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
