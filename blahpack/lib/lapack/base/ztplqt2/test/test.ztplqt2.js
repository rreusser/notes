

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztplqt2 = require( './../lib/ztplqt2.js' );


// TESTS //

test( 'ztplqt2 is a function', function t() {
	assert.strictEqual( typeof ztplqt2, 'function', 'is a function' );
});

test( 'ztplqt2 has expected arity', function t() {
	assert.strictEqual( ztplqt2.length, 10, 'has expected arity' );
});

test( 'ztplqt2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztplqt2( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztplqt2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztplqt2( 'row-major', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ztplqt2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztplqt2( 'row-major', 2, -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

