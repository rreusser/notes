/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zptsv = require( './../lib/zptsv.js' );


// TESTS //

test( 'zptsv is a function', function t() {
	assert.strictEqual( typeof zptsv, 'function', 'is a function' );
});

test( 'zptsv has expected arity', function t() {
	assert.strictEqual( zptsv.length, 8, 'has expected arity' );
});

test( 'zptsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zptsv( -1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zptsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zptsv( new Float64Array( 4 ), -1, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
