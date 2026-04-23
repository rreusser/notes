

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtgevc = require( './../lib/dtgevc.js' );


// TESTS //

test( 'dtgevc is a function', function t() {
	assert.strictEqual( typeof dtgevc, 'function', 'is a function' );
});

test( 'dtgevc has expected arity', function t() {
	assert.strictEqual( dtgevc.length, 18, 'has expected arity' );
});

test( 'dtgevc throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtgevc( 'invalid', 'left', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtgevc throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dtgevc( 'row-major', 'invalid', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtgevc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtgevc( 'row-major', 'left', 'no-transpose', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dtgevc throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtgevc( 'row-major', 'left', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

