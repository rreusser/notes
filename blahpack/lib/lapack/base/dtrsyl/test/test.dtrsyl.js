/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrsyl = require( './../lib/dtrsyl.js' );


// TESTS //

test( 'dtrsyl is a function', function t() {
	assert.strictEqual( typeof dtrsyl, 'function', 'is a function' );
});

test( 'dtrsyl has expected arity', function t() {
	assert.strictEqual( dtrsyl.length, 12, 'has expected arity' );
});

test( 'dtrsyl throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtrsyl( 2, 2, 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});

test( 'dtrsyl throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtrsyl( 2, 2, 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});
