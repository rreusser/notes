/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var idamax = require( './../lib/idamax.js' );


// TESTS //

test( 'idamax is a function', function t() {
	assert.strictEqual( typeof idamax, 'function', 'is a function' );
});

test( 'idamax has expected arity', function t() {
	assert.strictEqual( idamax.length, 3, 'has expected arity' );
});

test( 'idamax throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		idamax( -1, 2, 1 );
	}, RangeError );
});
