/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsyrfs = require( './../lib/zsyrfs.js' );


// TESTS //

test( 'zsyrfs is a function', function t() {
	assert.strictEqual( typeof zsyrfs, 'function', 'is a function' );
});

test( 'zsyrfs has expected arity', function t() {
	assert.strictEqual( zsyrfs.length, 2, 'has expected arity' );
});
