/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgtrfs = require( './../lib/zgtrfs.js' );


// TESTS //

test( 'zgtrfs is a function', function t() {
	assert.strictEqual( typeof zgtrfs, 'function', 'is a function' );
});

test( 'zgtrfs has expected arity', function t() {
	assert.strictEqual( zgtrfs.length, 31, 'has expected arity' );
});

test( 'zgtrfs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zgtrfs( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgtrfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgtrfs( 'no-transpose', -1, 2, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgtrfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zgtrfs( 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
