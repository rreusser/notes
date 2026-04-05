/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlansb = require( './../lib/dlansb.js' );


// TESTS //

test( 'dlansb is a function', function t() {
	assert.strictEqual( typeof dlansb, 'function', 'is a function' );
});

test( 'dlansb has expected arity', function t() {
	assert.strictEqual( dlansb.length, 7, 'has expected arity' );
});

test( 'dlansb throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlansb( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dlansb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlansb( 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});

test( 'dlansb throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dlansb( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
