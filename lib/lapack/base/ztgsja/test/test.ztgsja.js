/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztgsja = require( './../lib/ztgsja.js' );


// TESTS //

test( 'ztgsja is a function', function t() {
	assert.strictEqual( typeof ztgsja, 'function', 'is a function' );
});

test( 'ztgsja has expected arity', function t() {
	assert.strictEqual( ztgsja.length, 24, 'has expected arity' );
});

test( 'ztgsja throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztgsja( 2, 2, 2, -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ztgsja throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztgsja( 2, 2, 2, new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ztgsja throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		ztgsja( 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
