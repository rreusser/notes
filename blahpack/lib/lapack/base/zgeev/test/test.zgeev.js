/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgeev = require( './../lib/zgeev.js' );


// TESTS //

test( 'zgeev is a function', function t() {
	assert.strictEqual( typeof zgeev, 'function', 'is a function' );
});

test( 'zgeev has expected arity', function t() {
	assert.strictEqual( zgeev.length, 16, 'has expected arity' );
});

test( 'zgeev throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgeev( 2, 2, -1, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
