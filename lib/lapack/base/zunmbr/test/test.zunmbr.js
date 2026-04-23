/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zunmbr = require( './../lib/zunmbr.js' );


// TESTS //

test( 'zunmbr is a function', function t() {
	assert.strictEqual( typeof zunmbr, 'function', 'is a function' );
});

test( 'zunmbr has expected arity', function t() {
	assert.strictEqual( zunmbr.length, 15, 'has expected arity' );
});

test( 'zunmbr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zunmbr( 'invalid', 2, 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunmbr throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zunmbr( 'row-major', 2, 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunmbr throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zunmbr( 'row-major', 2, 'left', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunmbr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zunmbr( 'row-major', 2, 'left', 'no-transpose', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunmbr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zunmbr( 'row-major', 2, 'left', 'no-transpose', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunmbr throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zunmbr( 'row-major', 2, 'left', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
