/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsprfs = require( './../lib/dsprfs.js' );


// TESTS //

test( 'dsprfs is a function', function t() {
	assert.strictEqual( typeof dsprfs, 'function', 'is a function' );
});

test( 'dsprfs has expected arity', function t() {
	assert.strictEqual( dsprfs.length, 21, 'has expected arity' );
});

test( 'dsprfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsprfs( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsprfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsprfs( 'upper', -1, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dsprfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dsprfs( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
