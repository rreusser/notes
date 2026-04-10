/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zggsvp3 = require( './../lib/base.js' );
var ndarrayFn = require( './../lib/ndarray.js' );


// TESTS //

test( 'base is a function', function t() {
	assert.strictEqual( typeof zggsvp3, 'function', 'is a function' );
});

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarrayFn, 'function', 'is a function' );
});
