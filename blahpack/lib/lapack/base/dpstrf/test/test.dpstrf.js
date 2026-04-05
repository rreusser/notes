/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpstrf = require( './../lib/dpstrf.js' );


// TESTS //

test( 'dpstrf is a function', function t() {
	assert.strictEqual( typeof dpstrf, 'function', 'is a function' );
});

test( 'dpstrf has expected arity', function t() {
	assert.strictEqual( dpstrf.length, 9, 'has expected arity' );
});

test( 'dpstrf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dpstrf( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dpstrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpstrf( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dpstrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpstrf( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
