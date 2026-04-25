/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dtgsna = require( './../lib/ndarray.js' );
var ndarrayFn = require( './../lib/ndarray.js' );


// TESTS //

test( 'base is a function', function t() {
	assert.strictEqual( typeof dtgsna, 'function', 'is a function' );
});

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarrayFn, 'function', 'is a function' );
});

test( 'ndarray throws TypeError for invalid job', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'bogus', 'all', null, 1, 0, 0, null, 1, 1, 0, null, 1, 1, 0, null, 1, 1, 0, null, 1, 1, 0, null, 1, 0, null, 1, 0, 0, null, null, 1, 0, 1, null, 1, 0 );
	}, TypeError );
});

test( 'ndarray throws TypeError for invalid howmny', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'both', 'bogus', null, 1, 0, 0, null, 1, 1, 0, null, 1, 1, 0, null, 1, 1, 0, null, 1, 1, 0, null, 1, 0, null, 1, 0, 0, null, null, 1, 0, 1, null, 1, 0 );
	}, TypeError );
});

test( 'ndarray throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ndarrayFn( 'both', 'all', null, 1, 0, -1, null, 1, 1, 0, null, 1, 1, 0, null, 1, 1, 0, null, 1, 1, 0, null, 1, 0, null, 1, 0, 0, null, null, 1, 0, 1, null, 1, 0 );
	}, RangeError );
});
