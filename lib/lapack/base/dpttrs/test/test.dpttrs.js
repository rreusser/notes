/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpttrs = require( './../lib/dpttrs.js' );


// TESTS //

test( 'dpttrs is a function', function t() {
	assert.strictEqual( typeof dpttrs, 'function', 'is a function' );
});

test( 'dpttrs has expected arity', function t() {
	assert.strictEqual( dpttrs.length, 8, 'has expected arity' );
});

test( 'dpttrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpttrs( -1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dpttrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dpttrs( new Float64Array( 4 ), -1, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
