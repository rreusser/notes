/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlag2 = require( './../lib/dlag2.js' );


// TESTS //

test( 'dlag2 is a function', function t() {
	assert.strictEqual( typeof dlag2, 'function', 'is a function' );
});

test( 'dlag2 has expected arity', function t() {
	assert.strictEqual( dlag2.length, 6, 'has expected arity' );
});

test( 'dlag2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlag2( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});
