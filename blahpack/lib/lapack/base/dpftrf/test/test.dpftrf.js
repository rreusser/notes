/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpftrf = require( './../lib/dpftrf.js' );


// TESTS //

test( 'dpftrf is a function', function t() {
	assert.strictEqual( typeof dpftrf, 'function', 'is a function' );
});

test( 'dpftrf has expected arity', function t() {
	assert.strictEqual( dpftrf.length, 4, 'has expected arity' );
});

test( 'dpftrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpftrf( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dpftrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpftrf( 2, 'upper', -1, new Float64Array( 4 ) );
	}, RangeError );
});
