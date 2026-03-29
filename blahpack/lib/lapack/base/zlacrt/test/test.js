/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlacrt = require( './../lib' );
var base = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlacrt.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = {};
(function loadFixtures() {
	var i;
	var d;
	for ( i = 0; i < lines.length; i++ ) {
		d = JSON.parse( lines[ i ] );
		fixture[ d.name ] = d;
	}
})();


// FUNCTIONS //

/**
* @private
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* @private
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* @private
*/
function toFloat64( arr ) {
	return Array.prototype.slice.call( reinterpret( arr, 0 ) );
}


// TESTS //

test( 'zlacrt: main export is a function', function t() {
	assert.strictEqual( typeof zlacrt, 'function' );
});

test( 'zlacrt: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlacrt.ndarray, 'function' );
});

test( 'zlacrt: base is a function', function t() {
	assert.strictEqual( typeof base, 'function' );
});

test( 'zlacrt: basic (N=3, c=(0.6,0.1), s=(0.8,0.2))', function t() {
	var result;
	var tc;
	var cx;
	var cy;
	var c;
	var s;

	tc = fixture.basic;
	cx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	cy = new Complex128Array( [ 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 ] );
	c = new Complex128( 0.6, 0.1 );
	s = new Complex128( 0.8, 0.2 );
	result = base( 3, cx, 1, 0, cy, 1, 0, c, s );
	assert.strictEqual( result, cx );
	assertArrayClose( toFloat64( cx ), tc.cx, 1e-14, 'cx' );
	assertArrayClose( toFloat64( cy ), tc.cy, 1e-14, 'cy' );
});

test( 'zlacrt: n=0 is a no-op', function t() {
	var result;
	var tc;
	var cx;
	var cy;
	var c;
	var s;

	tc = fixture.n_zero;
	cx = new Complex128Array( [ 1.0, 2.0 ] );
	cy = new Complex128Array( [ 3.0, 4.0 ] );
	c = new Complex128( 0.6, 0.1 );
	s = new Complex128( 0.8, 0.2 );
	result = base( 0, cx, 1, 0, cy, 1, 0, c, s );
	assert.strictEqual( result, cx );
	assertArrayClose( toFloat64( cx ), tc.cx, 1e-14, 'cx unchanged' );
	assertArrayClose( toFloat64( cy ), tc.cy, 1e-14, 'cy unchanged' );
});

test( 'zlacrt: n=1 with purely imaginary s', function t() {
	var tc;
	var cx;
	var cy;
	var c;
	var s;

	tc = fixture.n_one;
	cx = new Complex128Array( [ 2.0, 3.0 ] );
	cy = new Complex128Array( [ 4.0, 5.0 ] );
	c = new Complex128( 1.0, 0.0 );
	s = new Complex128( 0.0, 1.0 );
	base( 1, cx, 1, 0, cy, 1, 0, c, s );
	assertArrayClose( toFloat64( cx ), tc.cx, 1e-14, 'cx' );
	assertArrayClose( toFloat64( cy ), tc.cy, 1e-14, 'cy' );
});

test( 'zlacrt: identity rotation c=(1,0), s=(0,0)', function t() {
	var tc;
	var cx;
	var cy;
	var c;
	var s;

	tc = fixture.identity;
	cx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	cy = new Complex128Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	c = new Complex128( 1.0, 0.0 );
	s = new Complex128( 0.0, 0.0 );
	base( 2, cx, 1, 0, cy, 1, 0, c, s );
	assertArrayClose( toFloat64( cx ), tc.cx, 1e-14, 'cx unchanged' );
	assertArrayClose( toFloat64( cy ), tc.cy, 1e-14, 'cy unchanged' );
});

test( 'zlacrt: swap rotation c=(0,0), s=(1,0)', function t() {
	var tc;
	var cx;
	var cy;
	var c;
	var s;

	tc = fixture.swap;
	cx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	cy = new Complex128Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	c = new Complex128( 0.0, 0.0 );
	s = new Complex128( 1.0, 0.0 );
	base( 2, cx, 1, 0, cy, 1, 0, c, s );
	assertArrayClose( toFloat64( cx ), tc.cx, 1e-14, 'cx = old cy' );
	assertArrayClose( toFloat64( cy ), tc.cy, 1e-14, 'cy = -old cx' );
});

test( 'zlacrt: non-unit strides (incx=2, incy=2)', function t() {
	var tc;
	var cx;
	var cy;
	var c;
	var s;

	tc = fixture.stride;
	cx = new Complex128Array( [ 1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0, 5.0, 6.0 ] );
	cy = new Complex128Array( [ 7.0, 8.0, 88.0, 88.0, 9.0, 10.0, 88.0, 88.0, 11.0, 12.0 ] );
	c = new Complex128( 0.6, 0.1 );
	s = new Complex128( 0.8, 0.2 );
	base( 3, cx, 2, 0, cy, 2, 0, c, s );
	assertArrayClose( toFloat64( cx ), tc.cx, 1e-14, 'cx' );
	assertArrayClose( toFloat64( cy ), tc.cy, 1e-14, 'cy' );
});

test( 'zlacrt: negative stride (incx=-1, incy=1)', function t() {
	var tc;
	var cx;
	var cy;
	var c;
	var s;

	// Fortran with incx=-1, n=3: starts at index (-3+1)*(-1)+1 = 3 (1-based) = 2 (0-based)
	tc = fixture.neg_stride;
	cx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	cy = new Complex128Array( [ 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 ] );
	c = new Complex128( 0.6, 0.1 );
	s = new Complex128( 0.8, 0.2 );
	base( 3, cx, -1, 2, cy, 1, 0, c, s );
	assertArrayClose( toFloat64( cx ), tc.cx, 1e-14, 'cx' );
	assertArrayClose( toFloat64( cy ), tc.cy, 1e-14, 'cy' );
});

test( 'zlacrt: purely imaginary c and s', function t() {
	var tc;
	var cx;
	var cy;
	var c;
	var s;

	tc = fixture.imag_cs;
	cx = new Complex128Array( [ 1.0, 0.0, 0.0, 1.0 ] );
	cy = new Complex128Array( [ 1.0, 0.0, 0.0, 1.0 ] );
	c = new Complex128( 0.0, 1.0 );
	s = new Complex128( 0.0, 1.0 );
	base( 2, cx, 1, 0, cy, 1, 0, c, s );
	assertArrayClose( toFloat64( cx ), tc.cx, 1e-14, 'cx' );
	assertArrayClose( toFloat64( cy ), tc.cy, 1e-14, 'cy' );
});

test( 'zlacrt: mixed strides (incx=2, incy=-1)', function t() {
	var tc;
	var cx;
	var cy;
	var c;
	var s;

	// Fortran: incx=2, incy=-1, n=2.
	// IX starts at 1 (0-based: 0), stride 2.
	// IY starts at (-2+1)*(-1)+1 = 2 (1-based) = 1 (0-based), stride -1.
	tc = fixture.mixed_stride;
	cx = new Complex128Array( [ 1.0, 1.0, 99.0, 99.0, 2.0, 2.0 ] );
	cy = new Complex128Array( [ 3.0, 3.0, 4.0, 4.0 ] );
	c = new Complex128( 0.5, 0.5 );
	s = new Complex128( 0.5, -0.5 );
	base( 2, cx, 2, 0, cy, -1, 1, c, s );
	assertArrayClose( toFloat64( cx ), tc.cx, 1e-14, 'cx' );
	assertArrayClose( toFloat64( cy ), tc.cy, 1e-14, 'cy' );
});

test( 'zlacrt: negative N returns cx unchanged', function t() {
	var result;
	var cx;
	var cy;
	var c;
	var s;

	cx = new Complex128Array( [ 1.0, 2.0 ] );
	cy = new Complex128Array( [ 3.0, 4.0 ] );
	c = new Complex128( 0.6, 0.1 );
	s = new Complex128( 0.8, 0.2 );
	result = base( -1, cx, 1, 0, cy, 1, 0, c, s );
	assert.strictEqual( result, cx );
	assertArrayClose( toFloat64( cx ), [ 1.0, 2.0 ], 1e-14, 'cx unchanged' );
	assertArrayClose( toFloat64( cy ), [ 3.0, 4.0 ], 1e-14, 'cy unchanged' );
});

test( 'zlacrt: with offset (offsetX=1, offsetY=1)', function t() {
	var cx;
	var cy;
	var c;
	var s;

	// Pad the arrays and use offset to skip the first element
	cx = new Complex128Array( [ 99.0, 99.0, 1.0, 2.0, 3.0, 4.0 ] );
	cy = new Complex128Array( [ 88.0, 88.0, 5.0, 6.0, 7.0, 8.0 ] );
	c = new Complex128( 1.0, 0.0 );
	s = new Complex128( 0.0, 0.0 );
	base( 2, cx, 1, 1, cy, 1, 1, c, s );

	// Identity rotation: arrays should be unchanged
	assertArrayClose( toFloat64( cx ), [ 99.0, 99.0, 1.0, 2.0, 3.0, 4.0 ], 1e-14, 'cx with offset' );
	assertArrayClose( toFloat64( cy ), [ 88.0, 88.0, 5.0, 6.0, 7.0, 8.0 ], 1e-14, 'cy with offset' );
});

test( 'zlacrt: ndarray wrapper validates N', function t() {
	var cx;
	var cy;
	var c;
	var s;

	cx = new Complex128Array( [ 1.0, 2.0 ] );
	cy = new Complex128Array( [ 3.0, 4.0 ] );
	c = new Complex128( 1.0, 0.0 );
	s = new Complex128( 0.0, 0.0 );
	assert.throws( function throws() {
		zlacrt.ndarray( -1, cx, 1, 0, cy, 1, 0, c, s );
	}, /nonnegative integer/ );
});
