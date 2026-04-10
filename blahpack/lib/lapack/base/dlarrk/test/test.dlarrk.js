

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarrk = require( './../lib/dlarrk.js' );


// TESTS //

test( 'dlarrk is a function', function t() {
	assert.strictEqual( typeof dlarrk, 'function', 'is a function' );
});

test( 'dlarrk has expected arity', function t() {
	assert.strictEqual( dlarrk.length, 12, 'has expected arity' );
});

test( 'dlarrk throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlarrk( 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dlarrk throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarrk( -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2 );
	}, RangeError );
});

