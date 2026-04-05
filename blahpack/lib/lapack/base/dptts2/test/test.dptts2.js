/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dptts2 = require( './../lib/dptts2.js' );


// TESTS //

test( 'dptts2 is a function', function t() {
	assert.strictEqual( typeof dptts2, 'function', 'is a function' );
});

test( 'dptts2 has expected arity', function t() {
	assert.strictEqual( dptts2.length, 8, 'has expected arity' );
});

test( 'dptts2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dptts2( -1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dptts2 throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dptts2( new Float64Array( 4 ), -1, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
