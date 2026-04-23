
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaein = require( './../lib/dlaein.js' );


// TESTS //

test( 'dlaein is a function', function t() {
	assert.strictEqual( typeof dlaein, 'function', 'is a function' );
});

test( 'dlaein has expected arity', function t() {
	assert.strictEqual( dlaein.length, 19, 'has expected arity' );
});

test( 'dlaein throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlaein( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dlaein throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaein( 'row-major', 2, 2, -1, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});
