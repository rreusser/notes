/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zungr2 = require( './../lib/zungr2.js' );


// TESTS //

test( 'zungr2 is a function', function t() {
	assert.strictEqual( typeof zungr2, 'function', 'is a function' );
});

test( 'zungr2 has expected arity', function t() {
	assert.strictEqual( zungr2.length, 10, 'has expected arity' );
});

test( 'zungr2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zungr2( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 ); // eslint-disable-line max-len
	}, /invalid argument/ );
});

test( 'zungr2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zungr2( 'row-major', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 ); // eslint-disable-line max-len
	}, /invalid argument/ );
});

test( 'zungr2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zungr2( 'row-major', 2, -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 ); // eslint-disable-line max-len
	}, /invalid argument/ );
});

test( 'zungr2 throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zungr2( 'row-major', 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 ); // eslint-disable-line max-len
	}, /invalid argument/ );
});
