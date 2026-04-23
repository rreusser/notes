

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztgsen = require( './../lib/ztgsen.js' );


// TESTS //

test( 'ztgsen is a function', function t() {
	assert.strictEqual( typeof ztgsen, 'function', 'is a function' );
});

test( 'ztgsen has expected arity', function t() {
	assert.strictEqual( ztgsen.length, 31, 'has expected arity' );
});

test( 'ztgsen throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztgsen( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2 );
	}, TypeError );
});

test( 'ztgsen throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztgsen( 'row-major', 2, 2, 2, new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2 );
	}, RangeError );
});

test( 'ztgsen throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztgsen( 'row-major', 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2 );
	}, RangeError );
});

