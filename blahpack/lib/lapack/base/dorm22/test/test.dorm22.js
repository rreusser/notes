

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dorm22 = require( './../lib/dorm22.js' );


// TESTS //

test( 'dorm22 is a function', function t() {
	assert.strictEqual( typeof dorm22, 'function', 'is a function' );
});

test( 'dorm22 has expected arity', function t() {
	assert.strictEqual( dorm22.length, 14, 'has expected arity' );
});

test( 'dorm22 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dorm22( 'invalid', 'left', 'no-transpose', 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'dorm22 throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dorm22( 'row-major', 'invalid', 'no-transpose', 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'dorm22 throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dorm22( 'row-major', 'left', 'invalid', 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'dorm22 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dorm22( 'row-major', 'left', 'no-transpose', -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});

test( 'dorm22 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dorm22( 'row-major', 'left', 'no-transpose', 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});

