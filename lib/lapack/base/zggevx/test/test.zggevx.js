
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zggevx = require( './../lib/zggevx.js' );


// TESTS //

test( 'zggevx is a function', function t() {
	assert.strictEqual( typeof zggevx, 'function', 'is a function' );
});

test( 'zggevx throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zggevx( 'invalid', 'none', 'no-vectors', 'no-vectors', 'none', 2, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, new Complex128Array( 2 ), 1, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Float64Array( 2 ), 1, new Float64Array( 2 ), 1, new Float64Array( 2 ), 1, new Float64Array( 2 ), 1 );
	}, TypeError );
});

test( 'zggevx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zggevx( 'column-major', 'none', 'no-vectors', 'no-vectors', 'none', -1, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, new Complex128Array( 2 ), 1, new Complex128Array( 4 ), 2, new Complex128Array( 4 ), 2, new Float64Array( 2 ), 1, new Float64Array( 2 ), 1, new Float64Array( 2 ), 1, new Float64Array( 2 ), 1 );
	}, RangeError );
});
