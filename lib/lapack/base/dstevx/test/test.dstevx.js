/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dstevx = require( './../lib/dstevx.js' );


// TESTS //

test( 'dstevx is a function', function t() {
	assert.strictEqual( typeof dstevx, 'function', 'is a function' );
});

test( 'dstevx has expected arity', function t() {
	assert.strictEqual( dstevx.length, 23, 'has expected arity' );
});

test( 'dstevx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dstevx( 2, 2, -1, 2, 1, 2, 1, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dstevx throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dstevx( 2, 2, new Float64Array( 4 ), 2, 1, 2, 1, 2, 2, 2, 2, 2, -1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
