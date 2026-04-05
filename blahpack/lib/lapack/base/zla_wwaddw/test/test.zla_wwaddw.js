/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_wwaddw = require( './../lib/zla_wwaddw.js' );


// TESTS //

test( 'zla_wwaddw is a function', function t() {
	assert.strictEqual( typeof zla_wwaddw, 'function', 'is a function' );
});

test( 'zla_wwaddw has expected arity', function t() {
	assert.strictEqual( zla_wwaddw.length, 4, 'has expected arity' );
});

test( 'zla_wwaddw throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zla_wwaddw( -1, 2, 2, 2 );
	}, RangeError );
});
