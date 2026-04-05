/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dstebz = require( './../lib/dstebz.js' );


// TESTS //

test( 'dstebz is a function', function t() {
	assert.strictEqual( typeof dstebz, 'function', 'is a function' );
});

test( 'dstebz has expected arity', function t() {
	assert.strictEqual( dstebz.length, 24, 'has expected arity' );
});

test( 'dstebz throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dstebz( 2, 'invalid', new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dstebz throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dstebz( 2, 'row-major', -1, 2, 2, 2, 2, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dstebz throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dstebz( 2, 'row-major', new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 1, 2, 1, -1, 2, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
