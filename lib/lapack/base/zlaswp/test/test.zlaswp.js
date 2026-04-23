/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaswp = require( './../lib/zlaswp.js' );


// TESTS //

test( 'zlaswp is a function', function t() {
	assert.strictEqual( typeof zlaswp, 'function', 'is a function' );
});

test( 'zlaswp has expected arity', function t() {
	assert.strictEqual( zlaswp.length, 9, 'has expected arity' );
});

test( 'zlaswp throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlaswp( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zlaswp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaswp( 'row-major', -1, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
