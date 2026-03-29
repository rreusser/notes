/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zrotg = require( './../lib/base.js' );


// FUNCTIONS //

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

/**
* Verify the Givens rotation properties:
*   1) c^2 + |s|^2 = 1
*   2) c*a + s*b = r
*   3) -conj(s)*a + c*b = 0
*
* @private
* @param {Array} aOrig - [aRe, aIm] original input
* @param {Array} bOrig - [bRe, bIm] original input
* @param {Complex128Array} aOut - a after zrotg (contains r)
* @param {Float64Array} cVal - c output
* @param {Complex128Array} sVal - s output
* @param {number} tol - tolerance
* @param {string} label - test label
*/
function verifyRotation( aOrig, bOrig, aOut, cVal, sVal, tol, label ) {
	var aRe = aOrig[ 0 ];
	var aIm = aOrig[ 1 ];
	var bRe = bOrig[ 0 ];
	var bIm = bOrig[ 1 ];
	var Rv = reinterpret( aOut, 0 );
	var rRe = Rv[ 0 ];
	var rIm = Rv[ 1 ];
	var cc = cVal[ 0 ];
	var sv = reinterpret( sVal, 0 );
	var sRe = sv[ 0 ];
	var sIm = sv[ 1 ];
	var scale;
	var norm;
	var zMag;
	var zRe;
	var zIm;

	// Property 1: c^2 + |s|^2 = 1
	norm = ( cc * cc ) + ( sRe * sRe ) + ( sIm * sIm );
	assertClose( norm, 1.0, tol, label + ': c^2 + |s|^2' );

	// Property 2: c*a + s*b = r
	zRe = ( cc * aRe ) + ( sRe * bRe ) - ( sIm * bIm );
	zIm = ( cc * aIm ) + ( sRe * bIm ) + ( sIm * bRe );
	assertClose( zRe, rRe, tol, label + ': (c*a + s*b).re = r.re' );
	assertClose( zIm, rIm, tol, label + ': (c*a + s*b).im = r.im' );

	// Property 3: -conj(s)*a + c*b = 0
	zRe = ( -sRe * aRe ) - ( sIm * aIm ) + ( cc * bRe );
	zIm = ( -sRe * aIm ) + ( sIm * aRe ) + ( cc * bIm );
	zMag = Math.sqrt( ( zRe * zRe ) + ( zIm * zIm ) );
	scale = Math.max( Math.sqrt( ( aRe * aRe ) + ( aIm * aIm ) ), Math.sqrt( ( bRe * bRe ) + ( bIm * bIm ) ), 1.0 );
	assert.ok( zMag / scale <= tol, label + ': -conj(s)*a + c*b = 0 (mag=' + zMag + ')' );
}


// TESTS //

test( 'zrotg: g = 0 (identity rotation)', function t() {
	var a = new Complex128Array( [ 3.0, 4.0 ] );
	var b = new Complex128Array( [ 0.0, 0.0 ] );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );
	var Av = reinterpret( a, 0 );
	var Sv = reinterpret( sv, 0 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.strictEqual( cv[ 0 ], 1.0 );
	assert.strictEqual( Sv[ 0 ], 0.0 );
	assert.strictEqual( Sv[ 1 ], 0.0 );
	assert.strictEqual( Av[ 0 ], 3.0 );
	assert.strictEqual( Av[ 1 ], 4.0 );
});

test( 'zrotg: f = 0, g pure real', function t() {
	var aOrig = [ 0.0, 0.0 ];
	var bOrig = [ 5.0, 0.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.strictEqual( cv[ 0 ], 0.0 );
	assertClose( reinterpret( a, 0 )[ 0 ], 5.0, 1e-14, 'r.re' );
	assertClose( reinterpret( a, 0 )[ 1 ], 0.0, 1e-14, 'r.im' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-14, 'f=0 g real' );
});

test( 'zrotg: f = 0, g pure imaginary', function t() {
	var aOrig = [ 0.0, 0.0 ];
	var bOrig = [ 0.0, 7.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.strictEqual( cv[ 0 ], 0.0 );
	assertClose( reinterpret( a, 0 )[ 0 ], 7.0, 1e-14, 'r = |g|' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-14, 'f=0 g imag' );
});

test( 'zrotg: f = 0, g negative real', function t() {
	var aOrig = [ 0.0, 0.0 ];
	var bOrig = [ -3.0, 0.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.strictEqual( cv[ 0 ], 0.0 );
	assertClose( reinterpret( a, 0 )[ 0 ], 3.0, 1e-14, 'r = |g|' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-14, 'f=0 g neg real' );
});

test( 'zrotg: f = 0, g general complex', function t() {
	var aOrig = [ 0.0, 0.0 ];
	var bOrig = [ 3.0, 4.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.strictEqual( cv[ 0 ], 0.0 );
	assertClose( reinterpret( a, 0 )[ 0 ], 5.0, 1e-14, 'r = |g|' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-14, 'f=0 g complex' );
});

test( 'zrotg: both zero', function t() {
	var a = new Complex128Array( [ 0.0, 0.0 ] );
	var b = new Complex128Array( [ 0.0, 0.0 ] );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.strictEqual( cv[ 0 ], 1.0 );
	assert.strictEqual( reinterpret( sv, 0 )[ 0 ], 0.0 );
	assert.strictEqual( reinterpret( sv, 0 )[ 1 ], 0.0 );
});

test( 'zrotg: general case, both nonzero', function t() {
	var aOrig = [ 1.0, 2.0 ];
	var bOrig = [ 3.0, 4.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.ok( cv[ 0 ] > 0.0, 'c > 0' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-14, 'general' );
});

test( 'zrotg: |f| > |g|', function t() {
	var aOrig = [ 10.0, 5.0 ];
	var bOrig = [ 1.0, -1.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-14, '|f|>|g|' );
});

test( 'zrotg: |f| < |g|', function t() {
	var aOrig = [ 0.1, -0.2 ];
	var bOrig = [ 5.0, 3.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-14, '|f|<|g|' );
});

test( 'zrotg: pure real inputs (matches drotg pattern)', function t() {
	var aOrig = [ 3.0, 0.0 ];
	var bOrig = [ 4.0, 0.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assertClose( cv[ 0 ], 3.0 / 5.0, 1e-14, 'c = a/r' );
	assertClose( reinterpret( sv, 0 )[ 0 ], 4.0 / 5.0, 1e-14, 's.re' );
	assertClose( reinterpret( sv, 0 )[ 1 ], 0.0, 1e-14, 's.im' );
	assertClose( reinterpret( a, 0 )[ 0 ], 5.0, 1e-14, 'r = 5' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-14, 'pure real' );
});

test( 'zrotg: pure imaginary inputs', function t() {
	var aOrig = [ 0.0, 3.0 ];
	var bOrig = [ 0.0, 4.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-14, 'pure imag' );
});

test( 'zrotg: large values (near overflow)', function t() {
	var big = 1e150;
	var aOrig = [ big, big ];
	var bOrig = [ big, -big ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );
	var Av;

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	Av = reinterpret( a, 0 );
	assert.ok( isFinite( cv[ 0 ] ), 'c is finite' );
	assert.ok( isFinite( Av[ 0 ] ), 'r.re is finite' );
	assert.ok( isFinite( Av[ 1 ] ), 'r.im is finite' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-10, 'large' );
});

test( 'zrotg: small values (near underflow)', function t() {
	var tiny = 1e-160;
	var aOrig = [ tiny, tiny ];
	var bOrig = [ tiny, -tiny ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.ok( isFinite( cv[ 0 ] ), 'c is finite' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-10, 'small' );
});

test( 'zrotg: f small, g large (separate scaling)', function t() {
	var aOrig = [ 1e-160, 1e-160 ];
	var bOrig = [ 1e100, 1e100 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.ok( isFinite( cv[ 0 ] ), 'c is finite' );
	assert.ok( isFinite( reinterpret( a, 0 )[ 0 ] ), 'r.re is finite' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-6, 'f small g large' );
});

test( 'zrotg: f = 0, g small complex (scaled path)', function t() {
	var aOrig = [ 0.0, 0.0 ];
	var bOrig = [ 1e-200, 1e-200 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.strictEqual( cv[ 0 ], 0.0 );
	assert.ok( isFinite( reinterpret( a, 0 )[ 0 ] ), 'r is finite' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-10, 'f=0 g small' );
});

test( 'zrotg: f = 0, g large complex (scaled path)', function t() {
	var aOrig = [ 0.0, 0.0 ];
	var bOrig = [ 1e200, -1e200 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.strictEqual( cv[ 0 ], 0.0 );
	assert.ok( isFinite( reinterpret( a, 0 )[ 0 ] ), 'r is finite' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-10, 'f=0 g large' );
});

test( 'zrotg: g >> f (f2 < h2*safmin path)', function t() {
	var aOrig = [ 1e-300, 1e-300 ];
	var bOrig = [ 1.0, 1.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.ok( isFinite( cv[ 0 ] ), 'c is finite' );
	assert.ok( cv[ 0 ] >= 0.0, 'c >= 0' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-10, 'g >> f' );
});

test( 'zrotg: with offsets', function t() {
	var aOrig = [ 1.0, 2.0 ];
	var bOrig = [ 3.0, 4.0 ];
	// a at offset 1 (complex element 1), b at offset 2
	var a = new Complex128Array( [ 99, 99, 1.0, 2.0 ] );
	var b = new Complex128Array( [ 99, 99, 99, 99, 3.0, 4.0 ] );
	var cv = new Float64Array( [ 99, 0.0 ] );
	var sv = new Complex128Array( [ 99, 99, 0.0, 0.0 ] );
	var Av;

	zrotg( a, 1, b, 2, cv, 1, sv, 1 );

	Av = reinterpret( a, 0 );
	verifyRotation( aOrig, bOrig, new Complex128Array( [ Av[ 2 ], Av[ 3 ] ] ), [ cv[ 1 ] ], new Complex128Array( [ reinterpret( sv, 0 )[ 2 ], reinterpret( sv, 0 )[ 3 ] ] ), 1e-14, 'offsets' );

	// Padding untouched
	assert.strictEqual( Av[ 0 ], 99 );
	assert.strictEqual( Av[ 1 ], 99 );
	assert.strictEqual( cv[ 0 ], 99 );
});

test( 'zrotg: unscaled f2 < h2*safmin (f tiny, g normal)', function t() {
	// f2 ~ 1e-600, g2 ~ 2, h2 ~ 2, f2/h2 ~ 5e-601 < safmin ~ 2e-308
	var aOrig = [ 1e-300, 1e-300 ];
	var bOrig = [ 1.0, 1.0 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.ok( isFinite( cv[ 0 ] ), 'c is finite' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-10, 'unscaled f2<h2*safmin' );
});

test( 'zrotg: scaled path, f2 < h2*safmin, c < safmin', function t() {
	// Both in scaled range, but f is much smaller than g even after scaling
	// f ~ 1e-300, g ~ 1e-155 => scaled range (both < rtmin ~ 1e-154)
	// After scaling by u ~ g1 ~ 1e-155: gs ~ 1, fs ~ 1e-145
	// f2 ~ 1e-290, g2 ~ 1, h2 ~ 1, f2/h2 < safmin
	var aOrig = [ 1e-300, 0.5e-300 ];
	var bOrig = [ 0.7e-155, 0.7e-155 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.ok( isFinite( cv[ 0 ] ), 'c is finite' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-6, 'scaled f2<h2*safmin' );
});

test( 'zrotg: scaled path, h2 >= rtmax (s computed via r/h2)', function t() {
	// Need f2 > rtmin and h2 >= rtmax*2 after scaling
	// Both large: f ~ 1e153, g ~ 1e153 => triggers scaled path
	// After scaling by u: fs ~ 1, gs ~ 1, f2 ~ 1, h2 ~ 2
	// But rtmax = sqrt(safmax/4) ~ 1e153, rtmax*2 ~ 2e153
	// h2 ~ 2 < rtmax*2, so we need really large scaled values
	// Try: f and g just outside rtmax range
	var rtmax = Math.sqrt( 4.494e307 / 4.0 );
	var val = rtmax * 1.1;
	var aOrig = [ val, val * 0.5 ];
	var bOrig = [ val * 0.7, val * 0.3 ];
	var a = new Complex128Array( aOrig );
	var b = new Complex128Array( bOrig );
	var cv = new Float64Array( 1 );
	var sv = new Complex128Array( 1 );

	zrotg( a, 0, b, 0, cv, 0, sv, 0 );
	assert.ok( isFinite( cv[ 0 ] ), 'c is finite' );
	verifyRotation( aOrig, bOrig, a, cv, sv, 1e-10, 'h2 near rtmax' );
});
