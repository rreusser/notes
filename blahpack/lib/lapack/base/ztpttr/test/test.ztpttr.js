/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztpttr = require( './../lib/ztpttr.js' );


// TESTS //

test( 'ztpttr is a function', function t() {
	assert.strictEqual( typeof ztpttr, 'function', 'is a function' );
});

test( 'ztpttr has expected arity', function t() {
	assert.strictEqual( ztpttr.length, 6, 'has expected arity' );
});

test( 'ztpttr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztpttr( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztpttr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztpttr( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztpttr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztpttr( 'row-major', 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
