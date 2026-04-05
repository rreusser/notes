/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaswp = require( './../lib/dlaswp.js' );


// TESTS //

test( 'dlaswp is a function', function t() {
	assert.strictEqual( typeof dlaswp, 'function', 'is a function' );
});

test( 'dlaswp has expected arity', function t() {
	assert.strictEqual( dlaswp.length, 9, 'has expected arity' );
});

test( 'dlaswp throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlaswp( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'dlaswp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaswp( 'row-major', -1, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
