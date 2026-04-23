/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaorhr_col_getrfnp2 = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlaorhr_col_getrfnp2, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dlaorhr_col_getrfnp2.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'main export has expected arity', function t() {
	assert.strictEqual( dlaorhr_col_getrfnp2.length, 7, 'has expected arity' );
});

test( 'ndarray method has expected arity', function t() {
	assert.strictEqual( dlaorhr_col_getrfnp2.ndarray.length, 9, 'ndarray has expected arity' ); // eslint-disable-line max-len
});
