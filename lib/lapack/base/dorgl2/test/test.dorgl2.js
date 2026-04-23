/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dorgl2 = require( './../lib/dorgl2.js' );


// TESTS //

test( 'dorgl2 is a function', function t() {
	assert.strictEqual( typeof dorgl2, 'function', 'is a function' );
});

test( 'dorgl2 has expected arity', function t() {
	assert.strictEqual( dorgl2.length, 10, 'has expected arity' );
});

test( 'dorgl2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dorgl2( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dorgl2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dorgl2( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorgl2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dorgl2( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dorgl2 throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dorgl2( 'row-major', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
