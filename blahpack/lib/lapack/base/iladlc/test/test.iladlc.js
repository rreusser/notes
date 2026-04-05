/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var iladlc = require( './../lib/iladlc.js' );


// TESTS //

test( 'iladlc is a function', function t() {
	assert.strictEqual( typeof iladlc, 'function', 'is a function' );
});

test( 'iladlc has expected arity', function t() {
	assert.strictEqual( iladlc.length, 5, 'has expected arity' );
});

test( 'iladlc throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		iladlc( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'iladlc throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		iladlc( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'iladlc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		iladlc( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
