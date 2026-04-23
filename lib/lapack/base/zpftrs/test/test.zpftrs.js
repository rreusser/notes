/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpftrs = require( './../lib/zpftrs.js' );


// TESTS //

test( 'zpftrs is a function', function t() {
	assert.strictEqual( typeof zpftrs, 'function', 'is a function' );
});

test( 'zpftrs has expected arity', function t() {
	assert.strictEqual( zpftrs.length, 6, 'has expected arity' );
});

test( 'zpftrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpftrs( 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpftrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpftrs( 2, 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zpftrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zpftrs( 2, 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
