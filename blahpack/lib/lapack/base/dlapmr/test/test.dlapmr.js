/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlapmr = require( './../lib/dlapmr.js' );


// TESTS //

test( 'dlapmr is a function', function t() {
	assert.strictEqual( typeof dlapmr, 'function', 'is a function' );
});

test( 'dlapmr has expected arity', function t() {
	assert.strictEqual( dlapmr.length, 7, 'has expected arity' );
});

test( 'dlapmr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlapmr( 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});

test( 'dlapmr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlapmr( 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});
