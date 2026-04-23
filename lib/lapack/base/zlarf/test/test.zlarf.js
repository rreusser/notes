/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlarf = require( './../lib/zlarf.js' );


// TESTS //

test( 'zlarf is a function', function t() {
	assert.strictEqual( typeof zlarf, 'function', 'is a function' );
});

test( 'zlarf has expected arity', function t() {
	assert.strictEqual( zlarf.length, 12, 'has expected arity' );
});

test( 'zlarf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlarf( 'invalid', 'left', new Float64Array( 4 ), new Float64Array( 4 ), 2, 1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlarf throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zlarf( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlarf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlarf( 'row-major', 'left', -1, new Float64Array( 4 ), 2, 1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zlarf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarf( 'row-major', 'left', new Float64Array( 4 ), -1, 2, 1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
