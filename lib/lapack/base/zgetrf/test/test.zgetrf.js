/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgetrf = require( './../lib/zgetrf.js' );


// TESTS //

test( 'zgetrf is a function', function t() {
	assert.strictEqual( typeof zgetrf, 'function', 'is a function' );
});

test( 'zgetrf has expected arity', function t() {
	assert.strictEqual( zgetrf.length, 7, 'has expected arity' );
});

test( 'zgetrf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgetrf( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgetrf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgetrf( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgetrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgetrf( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
