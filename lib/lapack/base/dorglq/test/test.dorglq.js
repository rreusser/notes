/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dorglq = require( './../lib/dorglq.js' );


// TESTS //

test( 'dorglq is a function', function t() {
	assert.strictEqual( typeof dorglq, 'function', 'is a function' );
});

test( 'dorglq has expected arity', function t() {
	assert.strictEqual( dorglq.length, 10, 'has expected arity' );
});

test( 'dorglq throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dorglq( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dorglq throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dorglq( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorglq throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dorglq( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorglq throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dorglq( 'row-major', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
