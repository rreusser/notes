
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zla_gbrfsx_extended = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zla_gbrfsx_extended, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zla_gbrfsx_extended.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'main export and ndarray variant are distinct functions', function t() {
	assert.notStrictEqual( zla_gbrfsx_extended, zla_gbrfsx_extended.ndarray, 'distinct functions' ); // eslint-disable-line max-len
});
