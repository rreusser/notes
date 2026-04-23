

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zgges = require( './../lib/zgges.js' );


// FUNCTIONS //

/**
* Dummy selection function.
*
* @private
* @returns {boolean} false
*/
function nosel() {
	return false;
}


// TESTS //

test( 'zgges is a function', function t() {
	assert.strictEqual( typeof zgges, 'function', 'is a function' );
});

test( 'zgges has expected arity', function t() {
	assert.strictEqual( zgges.length, 16, 'has expected arity' );
});

test( 'zgges throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgges( 'invalid', 'no-vectors', 'no-vectors', 'not-sorted', nosel, 2, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), new Complex128Array( 2 ), new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'zgges throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgges( 'column-major', 'no-vectors', 'no-vectors', 'not-sorted', nosel, -1, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), new Complex128Array( 2 ), new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2 ); // eslint-disable-line max-len
	}, RangeError );
});

