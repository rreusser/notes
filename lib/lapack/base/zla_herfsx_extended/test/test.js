
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlaHerfsxExtended = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zlaHerfsxExtended, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zlaHerfsxExtended.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'ndarray method is distinct from main export', function t() {
	assert.notStrictEqual( zlaHerfsxExtended, zlaHerfsxExtended.ndarray, 'main export is not the ndarray method' ); // eslint-disable-line max-len
});
