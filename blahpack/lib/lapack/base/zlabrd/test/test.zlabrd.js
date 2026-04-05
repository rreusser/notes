/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlabrd = require( './../lib/zlabrd.js' );


// TESTS //

test( 'zlabrd is a function', function t() {
	assert.strictEqual( typeof zlabrd, 'function', 'is a function' );
});

test( 'zlabrd has expected arity', function t() {
	assert.strictEqual( zlabrd.length, 18, 'has expected arity' );
});

test( 'zlabrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlabrd( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlabrd throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlabrd( 'row-major', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlabrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlabrd( 'row-major', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
