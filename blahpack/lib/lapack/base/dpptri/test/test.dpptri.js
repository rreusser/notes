/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpptri = require( './../lib/dpptri.js' );


// TESTS //

test( 'dpptri is a function', function t() {
	assert.strictEqual( typeof dpptri, 'function', 'is a function' );
});

test( 'dpptri has expected arity', function t() {
	assert.strictEqual( dpptri.length, 3, 'has expected arity' );
});

test( 'dpptri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpptri( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dpptri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpptri( 'upper', -1, new Float64Array( 4 ) );
	}, RangeError );
});
