/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqps = require( './../lib/dlaqps.js' );


// TESTS //

test( 'dlaqps is a function', function t() {
	assert.strictEqual( typeof dlaqps, 'function', 'is a function' );
});

test( 'dlaqps has expected arity', function t() {
	assert.strictEqual( dlaqps.length, 18, 'has expected arity' );
});

test( 'dlaqps throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlaqps( -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dlaqps throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaqps( new Float64Array( 4 ), -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
