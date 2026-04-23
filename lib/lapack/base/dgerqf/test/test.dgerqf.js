/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgerqf = require( './../lib/dgerqf.js' );


// TESTS //

test( 'dgerqf is a function', function t() {
	assert.strictEqual( typeof dgerqf, 'function', 'is a function' );
});

test( 'dgerqf has expected arity', function t() {
	assert.strictEqual( dgerqf.length, 8, 'has expected arity' );
});

test( 'dgerqf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgerqf( -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dgerqf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgerqf( new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
