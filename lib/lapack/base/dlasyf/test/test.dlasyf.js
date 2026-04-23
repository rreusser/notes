/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasyf = require( './../lib/dlasyf.js' );


// TESTS //

test( 'dlasyf is a function', function t() {
	assert.strictEqual( typeof dlasyf, 'function', 'is a function' );
});

test( 'dlasyf has expected arity', function t() {
	assert.strictEqual( dlasyf.length, 10, 'has expected arity' );
});

test( 'dlasyf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlasyf( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlasyf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlasyf( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlasyf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlasyf( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
