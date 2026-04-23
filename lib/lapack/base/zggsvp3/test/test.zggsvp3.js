/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len, max-params */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zggsvp3 = require( './../lib/zggsvp3.js' );


// FUNCTIONS //

/**
* Run a zggsvp3 call with the supplied order, M, N, expecting it to throw.
*
* @private
* @param {string} order - storage order
* @param {integer} M - rows
* @param {integer} N - columns
*/
function callZggsvp3( order, M, N ) {
	zggsvp3( order, 'compute-U', 'compute-V', 'compute-Q', M, 2, N, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, 1e-8, 1e-8, [ 0 ], [ 0 ], new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Int32Array( 2 ), 1, 0, new Float64Array( 4 ), 1, new Complex128Array( 2 ), 1, new Complex128Array( 100 ), 1, 100 );
}


// TESTS //

test( 'zggsvp3 is a function', function t() {
	assert.strictEqual( typeof zggsvp3, 'function', 'is a function' );
});

test( 'zggsvp3 throws TypeError for invalid order', function t() {
	var threw = false;
	try {
		callZggsvp3( 'invalid', 2, 2 );
	} catch ( err ) {
		threw = ( err instanceof TypeError );
	}
	assert.ok( threw, 'throws TypeError for invalid order' );
});

test( 'zggsvp3 throws RangeError for negative M', function t() {
	var threw = false;
	try {
		callZggsvp3( 'column-major', -1, 2 );
	} catch ( err ) {
		threw = ( err instanceof RangeError );
	}
	assert.ok( threw, 'throws RangeError for negative M' );
});

test( 'zggsvp3 throws RangeError for negative N', function t() {
	var threw = false;
	try {
		callZggsvp3( 'column-major', 2, -1 );
	} catch ( err ) {
		threw = ( err instanceof RangeError );
	}
	assert.ok( threw, 'throws RangeError for negative N' );
});
