/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zlaunhr_col_getrfnp2 = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zlaunhr_col_getrfnp2, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zlaunhr_col_getrfnp2.ndarray, 'function', 'has ndarray method' );
});

test( 'main export has expected arity', function t() {
	assert.strictEqual( zlaunhr_col_getrfnp2.length, 7, 'has expected arity' );
});

test( 'main throws TypeError for invalid order', function t() {
	assert.throws( function fn() {
		zlaunhr_col_getrfnp2( 'invalid', 1, 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1 );
	}, TypeError );
});

test( 'main throws RangeError for negative M', function t() {
	assert.throws( function fn() {
		zlaunhr_col_getrfnp2( 'column-major', -1, 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1 );
	}, RangeError );
});

test( 'main throws RangeError for negative N', function t() {
	assert.throws( function fn() {
		zlaunhr_col_getrfnp2( 'column-major', 1, -1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1 );
	}, RangeError );
});

test( 'main throws RangeError when LDA < max(1,M) for column-major', function t() {
	assert.throws( function fn() {
		zlaunhr_col_getrfnp2( 'column-major', 3, 3, new Complex128Array( 9 ), 2, new Complex128Array( 3 ), 1 );
	}, RangeError );
});
