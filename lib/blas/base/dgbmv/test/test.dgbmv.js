/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgbmv = require( './../lib/dgbmv.js' );


// TESTS //

test( 'dgbmv is a function', function t() {
	assert.strictEqual( typeof dgbmv, 'function', 'is a function' );
});

test( 'dgbmv has expected arity', function t() {
	assert.strictEqual( dgbmv.length, 14, 'has expected arity' );
});

test( 'dgbmv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgbmv( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'dgbmv throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dgbmv( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, TypeError );
});

test( 'dgbmv throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgbmv( 'row-major', 'no-transpose', -1, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, RangeError );
});

test( 'dgbmv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgbmv( 'row-major', 'no-transpose', new Float64Array( 4 ), -1, 2, 2, 2, new Float64Array( 4 ), 2, 2, 1, 2, 2, 1 );
	}, RangeError );
});
