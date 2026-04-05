/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztgevc = require( './../lib/ztgevc.js' );


// TESTS //

test( 'ztgevc is a function', function t() {
	assert.strictEqual( typeof ztgevc, 'function', 'is a function' );
});

test( 'ztgevc has expected arity', function t() {
	assert.strictEqual( ztgevc.length, 21, 'has expected arity' );
});

test( 'ztgevc throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztgevc( 'invalid', 'left', 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztgevc throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		ztgevc( 'row-major', 'invalid', 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztgevc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztgevc( 'row-major', 'left', 2, new Float64Array( 4 ), 1, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'ztgevc throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztgevc( 'row-major', 'left', 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
