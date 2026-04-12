
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zheswapr = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zheswapr, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zheswapr.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'main and ndarray export are distinct functions', function t() {
	assert.notStrictEqual( zheswapr, zheswapr.ndarray, 'main and ndarray are distinct' ); // eslint-disable-line max-len
});
