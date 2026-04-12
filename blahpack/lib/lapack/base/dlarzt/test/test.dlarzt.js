/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarzt = require( './../lib/dlarzt.js' );


// TESTS //

test( 'dlarzt is a function', function t() {
	assert.strictEqual( typeof dlarzt, 'function', 'is a function' );
});

test( 'dlarzt has expected arity', function t() {
	assert.strictEqual( dlarzt.length, 11, 'has expected arity' );
});

test( 'dlarzt throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlarzt( 'invalid', 'no-transpose', 'no-transpose', 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlarzt throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarzt( 'row-major', 'no-transpose', 'no-transpose', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dlarzt throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dlarzt( 'row-major', 'no-transpose', 'no-transpose', 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
