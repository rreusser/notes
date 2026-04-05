/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zunglq = require( './../lib/zunglq.js' );


// TESTS //

test( 'zunglq is a function', function t() {
	assert.strictEqual( typeof zunglq, 'function', 'is a function' );
});

test( 'zunglq has expected arity', function t() {
	assert.strictEqual( zunglq.length, 10, 'has expected arity' );
});

test( 'zunglq throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zunglq( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zunglq throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zunglq( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunglq throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zunglq( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zunglq throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zunglq( 'row-major', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
