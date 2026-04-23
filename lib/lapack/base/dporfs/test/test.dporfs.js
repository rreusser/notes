/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dporfs = require( './../lib/dporfs.js' );


// TESTS //

test( 'dporfs is a function', function t() {
	assert.strictEqual( typeof dporfs, 'function', 'is a function' );
});

test( 'dporfs has expected arity', function t() {
	assert.strictEqual( dporfs.length, 15, 'has expected arity' );
});

test( 'dporfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dporfs( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dporfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dporfs( 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dporfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dporfs( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
