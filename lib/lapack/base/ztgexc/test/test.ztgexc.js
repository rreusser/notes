

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztgexc = require( './../lib/ztgexc.js' );


// TESTS //

test( 'ztgexc is a function', function t() {
	assert.strictEqual( typeof ztgexc, 'function', 'is a function' );
});

test( 'ztgexc has expected arity', function t() {
	assert.strictEqual( ztgexc.length, 14, 'has expected arity' );
});

test( 'ztgexc throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztgexc( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2 );
	}, TypeError );
});

test( 'ztgexc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztgexc( 'row-major', 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2 );
	}, RangeError );
});

