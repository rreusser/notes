/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztgsyl = require( './../lib/ztgsyl.js' );


// TESTS //

test( 'ztgsyl is a function', function t() {
	assert.strictEqual( typeof ztgsyl, 'function', 'is a function' );
});

test( 'ztgsyl has expected arity', function t() {
	assert.strictEqual( ztgsyl.length, 23, 'has expected arity' );
});

test( 'ztgsyl throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ztgsyl( 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztgsyl throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztgsyl( 'no-transpose', 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'ztgsyl throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztgsyl( 'no-transpose', 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
