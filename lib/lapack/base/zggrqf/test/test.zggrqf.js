/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zggrqf = require( './../lib/zggrqf.js' );


// TESTS //

test( 'zggrqf is a function', function t() {
	assert.strictEqual( typeof zggrqf, 'function', 'is a function' );
});

test( 'zggrqf has expected arity', function t() {
	assert.strictEqual( zggrqf.length, 14, 'has expected arity' );
});

test( 'zggrqf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zggrqf( -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zggrqf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zggrqf( new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
