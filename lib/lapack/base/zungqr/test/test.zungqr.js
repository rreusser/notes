/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zungqr = require( './../lib/zungqr.js' );


// TESTS //

test( 'zungqr is a function', function t() {
	assert.strictEqual( typeof zungqr, 'function', 'is a function' );
});

test( 'zungqr has expected arity', function t() {
	assert.strictEqual( zungqr.length, 10, 'has expected arity' );
});

test( 'zungqr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zungqr( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zungqr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zungqr( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zungqr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zungqr( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zungqr throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zungqr( 'row-major', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
