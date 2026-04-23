/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zggqrf = require( './../lib/zggqrf.js' );


// TESTS //

test( 'zggqrf is a function', function t() {
	assert.strictEqual( typeof zggqrf, 'function', 'is a function' );
});

test( 'zggqrf has expected arity', function t() {
	assert.strictEqual( zggqrf.length, 14, 'has expected arity' );
});

test( 'zggqrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zggqrf( -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zggqrf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zggqrf( new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
