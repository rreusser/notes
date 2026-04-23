/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpptrf = require( './../lib/dpptrf.js' );


// TESTS //

test( 'dpptrf is a function', function t() {
	assert.strictEqual( typeof dpptrf, 'function', 'is a function' );
});

test( 'dpptrf has expected arity', function t() {
	assert.strictEqual( dpptrf.length, 3, 'has expected arity' );
});

test( 'dpptrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpptrf( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dpptrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpptrf( 'upper', -1, new Float64Array( 4 ) );
	}, RangeError );
});
