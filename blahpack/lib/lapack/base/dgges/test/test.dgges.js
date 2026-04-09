

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgges = require( './../lib/dgges.js' );


// FUNCTIONS //

/**
* Noop selection function.
*
* @private
* @returns {boolean} false
*/
function noop() {
	return false;
}


// TESTS //

test( 'dgges is a function', function t() {
	assert.strictEqual( typeof dgges, 'function', 'is a function' );
});

test( 'dgges has expected arity', function t() {
	assert.strictEqual( dgges.length, 17, 'has expected arity' );
});

test( 'dgges throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgges( 'invalid', 'compute-vectors', 'compute-vectors', 'not-sorted', noop, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 2 ), new Float64Array( 2 ), new Float64Array( 2 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'dgges throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgges( 'row-major', 'compute-vectors', 'compute-vectors', 'not-sorted', noop, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 2 ), new Float64Array( 2 ), new Float64Array( 2 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'dgges throws RangeError for invalid LDA', function t() {
	assert.throws( function throws() {
		dgges( 'row-major', 'compute-vectors', 'compute-vectors', 'not-sorted', noop, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 2 ), new Float64Array( 2 ), new Float64Array( 2 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'dgges throws RangeError for invalid LDB', function t() {
	assert.throws( function throws() {
		dgges( 'row-major', 'compute-vectors', 'compute-vectors', 'not-sorted', noop, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 2 ), new Float64Array( 2 ), new Float64Array( 2 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 ); // eslint-disable-line max-len
	}, RangeError );
});
