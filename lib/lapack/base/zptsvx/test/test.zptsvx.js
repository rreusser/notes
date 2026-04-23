/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zptsvx = require( './../lib/zptsvx.js' );


// TESTS //

test( 'zptsvx is a function', function t() {
	assert.strictEqual( typeof zptsvx, 'function', 'is a function' );
});

test( 'zptsvx has expected arity', function t() {
	assert.strictEqual( zptsvx.length, 16, 'has expected arity' );
});

test( 'zptsvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zptsvx( 2, -1, 2, 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zptsvx throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zptsvx( 2, new Float64Array( 4 ), -1, 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
