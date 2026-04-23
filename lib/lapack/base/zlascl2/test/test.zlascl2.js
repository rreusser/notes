/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlascl2 = require( './../lib/zlascl2.js' );


// TESTS //

test( 'zlascl2 is a function', function t() {
	assert.strictEqual( typeof zlascl2, 'function', 'is a function' );
});

test( 'zlascl2 has expected arity', function t() {
	assert.strictEqual( zlascl2.length, 6, 'has expected arity' );
});

test( 'zlascl2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlascl2( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlascl2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlascl2( 'row-major', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlascl2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlascl2( 'row-major', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
