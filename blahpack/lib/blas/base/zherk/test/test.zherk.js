/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zherk = require( './../lib/zherk.js' );


// TESTS //

test( 'zherk is a function', function t() {
	assert.strictEqual( typeof zherk, 'function', 'is a function' );
});

test( 'zherk has expected arity', function t() {
	assert.strictEqual( zherk.length, 11, 'has expected arity' );
});

test( 'zherk throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zherk( 'invalid', 'upper', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zherk throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zherk( 'row-major', 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zherk throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zherk( 'row-major', 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zherk throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zherk( 'row-major', 'upper', 'no-transpose', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zherk throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zherk( 'row-major', 'upper', 'no-transpose', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
