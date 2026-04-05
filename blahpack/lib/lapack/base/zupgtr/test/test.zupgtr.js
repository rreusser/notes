/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zupgtr = require( './../lib/zupgtr.js' );


// TESTS //

test( 'zupgtr is a function', function t() {
	assert.strictEqual( typeof zupgtr, 'function', 'is a function' );
});

test( 'zupgtr has expected arity', function t() {
	assert.strictEqual( zupgtr.length, 8, 'has expected arity' );
});

test( 'zupgtr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zupgtr( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zupgtr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zupgtr( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zupgtr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zupgtr( 'row-major', 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
