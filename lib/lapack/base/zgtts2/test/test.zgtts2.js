/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgtts2 = require( './../lib/zgtts2.js' );


// TESTS //

test( 'zgtts2 is a function', function t() {
	assert.strictEqual( typeof zgtts2, 'function', 'is a function' );
});

test( 'zgtts2 has expected arity', function t() {
	assert.strictEqual( zgtts2.length, 15, 'has expected arity' );
});

test( 'zgtts2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgtts2( 2, -1, 2, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zgtts2 throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zgtts2( 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
