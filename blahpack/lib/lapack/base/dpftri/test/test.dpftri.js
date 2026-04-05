/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpftri = require( './../lib/dpftri.js' );


// TESTS //

test( 'dpftri is a function', function t() {
	assert.strictEqual( typeof dpftri, 'function', 'is a function' );
});

test( 'dpftri has expected arity', function t() {
	assert.strictEqual( dpftri.length, 4, 'has expected arity' );
});

test( 'dpftri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpftri( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dpftri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpftri( 2, 'upper', -1, new Float64Array( 4 ) );
	}, RangeError );
});
