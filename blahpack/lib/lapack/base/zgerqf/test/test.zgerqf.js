/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgerqf = require( './../lib/zgerqf.js' );


// TESTS //

test( 'zgerqf is a function', function t() {
	assert.strictEqual( typeof zgerqf, 'function', 'is a function' );
});

test( 'zgerqf has expected arity', function t() {
	assert.strictEqual( zgerqf.length, 9, 'has expected arity' );
});

test( 'zgerqf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgerqf( -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zgerqf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgerqf( new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
