/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpftrs = require( './../lib/dpftrs.js' );


// TESTS //

test( 'dpftrs is a function', function t() {
	assert.strictEqual( typeof dpftrs, 'function', 'is a function' );
});

test( 'dpftrs has expected arity', function t() {
	assert.strictEqual( dpftrs.length, 7, 'has expected arity' );
});

test( 'dpftrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpftrs( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpftrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpftrs( 2, 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dpftrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dpftrs( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
