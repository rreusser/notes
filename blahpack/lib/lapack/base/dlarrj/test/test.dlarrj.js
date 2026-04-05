/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarrj = require( './../lib/dlarrj.js' );


// TESTS //

test( 'dlarrj is a function', function t() {
	assert.strictEqual( typeof dlarrj, 'function', 'is a function' );
});

test( 'dlarrj has expected arity', function t() {
	assert.strictEqual( dlarrj.length, 19, 'has expected arity' );
});

test( 'dlarrj throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarrj( -1, 2, 1, new Float64Array( 4 ), 1, 2, 2, 2, 2, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, 2 );
	}, RangeError );
});
