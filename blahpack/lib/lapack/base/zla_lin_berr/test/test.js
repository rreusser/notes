/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zlaLinBerr = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zlaLinBerr, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zlaLinBerr.ndarray, 'function', 'has ndarray method' );
});

test( 'zlaLinBerr returns the backward error (main entry)', function t() {
	var berr = new Float64Array( 1 );
	var res = new Complex128Array([ 1.0, 2.0 ]);
	var ayb = new Float64Array([ 1.0 ]);

	zlaLinBerr( 1, 0, 1, res, 1, ayb, 1, berr, 1 );

	// CABS1(1+2i) = 3; safe1 ~ 0 so berr[0] ~= 3.0:
	assert.ok( Math.abs( berr[ 0 ] - 3.0 ) < 1e-12, 'berr[0] is approximately 3.0' );
});

test( 'zlaLinBerr.ndarray honours offsets', function t() {
	var berr = new Float64Array([ -1.0, -1.0 ]);
	var res = new Complex128Array([ 1.0, -2.0, 3.0, 4.0 ]);
	var ayb = new Float64Array([ 1.0, 1.0 ]);

	zlaLinBerr.ndarray( 1, 0, 1, res, 1, 1, ayb, 1, 1, berr, 1, 1 );

	// Uses second complex element (3+4i), CABS1 = 7; ayb=1 → 7:
	assert.ok( Math.abs( berr[ 1 ] - 7.0 ) < 1e-12, 'berr[1] is approximately 7.0' );
	assert.strictEqual( berr[ 0 ], -1.0, 'berr[0] is untouched' );
});
