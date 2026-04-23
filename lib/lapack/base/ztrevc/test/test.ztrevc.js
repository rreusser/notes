
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrevc = require( './../lib/ztrevc.js' );


// TESTS //

test( 'ztrevc is a function', function t() {
	assert.strictEqual( typeof ztrevc, 'function', 'is a function' );
});

test( 'ztrevc has expected arity', function t() {
	assert.strictEqual( ztrevc.length, 18, 'has expected arity' );
});

test( 'ztrevc throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztrevc( 'invalid', 'left', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrevc throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		ztrevc( 'row-major', 'invalid', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrevc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztrevc( 'row-major', 'left', 'no-transpose', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ztrevc throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztrevc( 'row-major', 'left', 'no-transpose', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
