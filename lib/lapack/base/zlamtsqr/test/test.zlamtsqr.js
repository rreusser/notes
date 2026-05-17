
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlamtsqr = require( './../lib/zlamtsqr.js' );


// TESTS //

test( 'zlamtsqr is a function', function t() {
	assert.strictEqual( typeof zlamtsqr, 'function', 'is a function' );
});

test( 'zlamtsqr has expected arity', function t() {
	assert.strictEqual( zlamtsqr.length, 17, 'has expected arity' );
});

test( 'zlamtsqr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlamtsqr( 'invalid', 'left', 'no-transpose', 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'zlamtsqr throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zlamtsqr( 'row-major', 'invalid', 'no-transpose', 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'zlamtsqr throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zlamtsqr( 'row-major', 'left', 'invalid', 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'zlamtsqr rejects plain "transpose" (Q is unitary)', function t() {
	assert.throws( function throws() {
		zlamtsqr( 'column-major', 'left', 'transpose', 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'zlamtsqr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlamtsqr( 'row-major', 'left', 'no-transpose', -1, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});

test( 'zlamtsqr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlamtsqr( 'row-major', 'left', 'no-transpose', 2, -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});

test( 'zlamtsqr throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zlamtsqr( 'row-major', 'left', 'no-transpose', 2, 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});
