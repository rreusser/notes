

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgbtrs = require( './../lib' );
var ndarrayFn = require( './../lib/ndarray.js' );

test( 'zgbtrs: main export is a function', function t() {
	assert.strictEqual( typeof zgbtrs, 'function' );
});

test( 'zgbtrs: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgbtrs.ndarray, 'function' );
});

// ndarray validation tests

test( 'zgbtrs: ndarray throws TypeError for invalid trans', function t() {
	var AB = new Float64Array( 16 );
	var IPIV = new Int32Array( 4 );
	var B = new Float64Array( 4 );
	assert.throws( function() {
		ndarrayFn( 'invalid', 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	}, TypeError );
});

test( 'zgbtrs: ndarray throws RangeError for negative N', function t() {
	var AB = new Float64Array( 16 );
	var IPIV = new Int32Array( 4 );
	var B = new Float64Array( 4 );
	assert.throws( function() {
		ndarrayFn( 'no-transpose', -1, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	}, RangeError );
});
