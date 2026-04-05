/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zungtr = require( './../lib/zungtr.js' );


// TESTS //

test( 'zungtr is a function', function t() {
	assert.strictEqual( typeof zungtr, 'function', 'is a function' );
});

test( 'zungtr has expected arity', function t() {
	assert.strictEqual( zungtr.length, 8, 'has expected arity' );
});

test( 'zungtr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zungtr( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zungtr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zungtr( 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
