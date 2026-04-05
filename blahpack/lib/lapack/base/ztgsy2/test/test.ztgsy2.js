/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztgsy2 = require( './../lib/ztgsy2.js' );


// TESTS //

test( 'ztgsy2 is a function', function t() {
	assert.strictEqual( typeof ztgsy2, 'function', 'is a function' );
});

test( 'ztgsy2 has expected arity', function t() {
	assert.strictEqual( ztgsy2.length, 19, 'has expected arity' );
});

test( 'ztgsy2 throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ztgsy2( 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'ztgsy2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztgsy2( 'no-transpose', 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});

test( 'ztgsy2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztgsy2( 'no-transpose', 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});
