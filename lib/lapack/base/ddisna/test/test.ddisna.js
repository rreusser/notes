

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ddisna = require( './../lib/ddisna.js' );


// TESTS //

test( 'ddisna is a function', function t() {
	assert.strictEqual( typeof ddisna, 'function', 'is a function' );
});

test( 'ddisna has expected arity', function t() {
	assert.strictEqual( ddisna.length, 7, 'has expected arity' );
});

test( 'ddisna throws RangeError for invalid job (via base.js)', function t() {
	var info = ddisna( 'invalid', 2, 2, new Float64Array( [ 1.0, 2.0 ] ), 1, new Float64Array( 2 ), 1 );
	assert.strictEqual( info, -1, 'returns -1 for invalid job' );
});

test( 'ddisna throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ddisna( 'eigenvalues', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ddisna throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ddisna( 'eigenvalues', 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

