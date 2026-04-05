/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zungql = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zungql, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zungql.ndarray, 'function', 'has ndarray method' );
});
