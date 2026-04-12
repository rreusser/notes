/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dla_syrcond = require( './../lib/dla_syrcond.js' );


// TESTS //

test( 'dla_syrcond is a function', function t() {
	assert.strictEqual( typeof dla_syrcond, 'function', 'is a function' );
});

test( 'dla_syrcond has expected arity', function t() {
	assert.strictEqual( dla_syrcond.length, 18, 'has expected arity' );
});

test( 'dla_syrcond throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dla_syrcond( 'invalid', 'upper', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dla_syrcond throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dla_syrcond( 'row-major', 'invalid', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dla_syrcond throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dla_syrcond( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});
