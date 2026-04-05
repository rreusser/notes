/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgbsv = require( './../lib/zgbsv.js' );


// TESTS //

test( 'zgbsv is a function', function t() {
	assert.strictEqual( typeof zgbsv, 'function', 'is a function' );
});

test( 'zgbsv has expected arity', function t() {
	assert.strictEqual( zgbsv.length, 10, 'has expected arity' );
});

test( 'zgbsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgbsv( -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zgbsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zgbsv( new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
