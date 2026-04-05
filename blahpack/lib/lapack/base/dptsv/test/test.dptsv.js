/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dptsv = require( './../lib/dptsv.js' );


// TESTS //

test( 'dptsv is a function', function t() {
	assert.strictEqual( typeof dptsv, 'function', 'is a function' );
});

test( 'dptsv has expected arity', function t() {
	assert.strictEqual( dptsv.length, 8, 'has expected arity' );
});

test( 'dptsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dptsv( -1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dptsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dptsv( new Float64Array( 4 ), -1, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
