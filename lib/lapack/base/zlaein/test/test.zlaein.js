/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaein = require( './../lib/zlaein.js' );


// TESTS //

test( 'zlaein is a function', function t() {
	assert.strictEqual( typeof zlaein, 'function', 'is a function' );
});

test( 'zlaein has expected arity', function t() {
	assert.strictEqual( zlaein.length, 15, 'has expected arity' );
});

test( 'zlaein throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlaein( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2 );
	}, TypeError );
});
