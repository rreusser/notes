/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgtsv = require( './../lib/zgtsv.js' );


// TESTS //

test( 'zgtsv is a function', function t() {
	assert.strictEqual( typeof zgtsv, 'function', 'is a function' );
});

test( 'zgtsv has expected arity', function t() {
	assert.strictEqual( zgtsv.length, 10, 'has expected arity' );
});

test( 'zgtsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgtsv( -1, 2, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zgtsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zgtsv( new Float64Array( 4 ), -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
