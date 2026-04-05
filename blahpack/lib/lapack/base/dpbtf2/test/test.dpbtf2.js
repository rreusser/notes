/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpbtf2 = require( './../lib/dpbtf2.js' );


// TESTS //

test( 'dpbtf2 is a function', function t() {
	assert.strictEqual( typeof dpbtf2, 'function', 'is a function' );
});

test( 'dpbtf2 has expected arity', function t() {
	assert.strictEqual( dpbtf2.length, 6, 'has expected arity' );
});

test( 'dpbtf2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dpbtf2( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpbtf2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpbtf2( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpbtf2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpbtf2( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
