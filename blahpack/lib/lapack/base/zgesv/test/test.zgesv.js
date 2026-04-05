/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgesv = require( './../lib/zgesv.js' );


// TESTS //

test( 'zgesv is a function', function t() {
	assert.strictEqual( typeof zgesv, 'function', 'is a function' );
});

test( 'zgesv has expected arity', function t() {
	assert.strictEqual( zgesv.length, 8, 'has expected arity' );
});

test( 'zgesv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgesv( -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zgesv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zgesv( new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
