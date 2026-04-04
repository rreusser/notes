
'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarscl2 = require( './../lib/base.js' );

// FIXTURES //

var basic_3x3 = require( './fixtures/basic_3x3.json' );
var single_element = require( './fixtures/single_element.json' );
var rect_2x3 = require( './fixtures/rect_2x3.json' );
var rect_3x2 = require( './fixtures/rect_3x2.json' );
var ldx_gt_m = require( './fixtures/ldx_gt_m.json' );

// FUNCTIONS //

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assert.ok(
			Math.abs( actual[ i ] - expected[ i ] ) <= tol * Math.max( Math.abs( expected[ i ] ), 1.0 ),
			msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ]
		);
	}
}

// TESTS //

test( 'zlarscl2 is a function', function t() {
	assert.strictEqual( typeof zlarscl2, 'function' );
});

test( 'zlarscl2: basic_3x3', function t() {
	var view;
	var tc = basic_3x3;
	var x = new Complex128Array( [
		1, 2, 3, 4, 5, 6,
		7, 8, 9, 10, 11, 12,
		13, 14, 15, 16, 17, 18
	] );
	var d = new Float64Array( [ 2, 3, 4 ] );
	zlarscl2( 3, 3, d, 1, 0, x, 1, 3, 0 );
	view = reinterpret( x, 0 );
	assertArrayClose( Array.from( view ), tc.x, 1e-14, 'x' );
});

test( 'zlarscl2: m_zero (quick return)', function t() {
	var view;
	var x = new Complex128Array( [ 99, 88 ] );
	var d = new Float64Array( [ 2, 3, 4 ] );
	zlarscl2( 0, 3, d, 1, 0, x, 1, 3, 0 );
	view = reinterpret( x, 0 );
	assert.strictEqual( view[ 0 ], 99.0 );
	assert.strictEqual( view[ 1 ], 88.0 );
});

test( 'zlarscl2: n_zero (quick return)', function t() {
	var view;
	var x = new Complex128Array( [ 99, 88 ] );
	var d = new Float64Array( [ 2, 3, 4 ] );
	zlarscl2( 3, 0, d, 1, 0, x, 1, 3, 0 );
	view = reinterpret( x, 0 );
	assert.strictEqual( view[ 0 ], 99.0 );
	assert.strictEqual( view[ 1 ], 88.0 );
});

test( 'zlarscl2: single_element', function t() {
	var view;
	var tc = single_element;
	var x = new Complex128Array( [ 5, -3 ] );
	var d = new Float64Array( [ 3 ] );
	zlarscl2( 1, 1, d, 1, 0, x, 1, 1, 0 );
	view = reinterpret( x, 0 );
	assertArrayClose( Array.from( view ), tc.x, 1e-14, 'x' );
});

test( 'zlarscl2: rect_2x3', function t() {
	var view;
	var tc = rect_2x3;
	var x = new Complex128Array( [
		1, 0.5, 2, 1,
		3, 1.5, 4, 2,
		5, 2.5, 6, 3
	] );
	var d = new Float64Array( [ 0.5, 2.0 ] );
	zlarscl2( 2, 3, d, 1, 0, x, 1, 2, 0 );
	view = reinterpret( x, 0 );
	assertArrayClose( Array.from( view ), tc.x, 1e-14, 'x' );
});

test( 'zlarscl2: rect_3x2', function t() {
	var view;
	var tc = rect_3x2;
	var x = new Complex128Array( [
		1, -1, 2, -2, 3, -3,
		4, -4, 5, -5, 6, -6
	] );
	var d = new Float64Array( [ 10, 20, 30 ] );
	zlarscl2( 3, 2, d, 1, 0, x, 1, 3, 0 );
	view = reinterpret( x, 0 );
	assertArrayClose( Array.from( view ), tc.x, 1e-14, 'x' );
});

test( 'zlarscl2: negative and zero values in D', function t() {
	var view;
	var x = new Complex128Array( [
		1, 2, 3, 4,
		5, 6, 7, 8
	] );
	var d = new Float64Array( [ -1, 0 ] );
	zlarscl2( 2, 2, d, 1, 0, x, 1, 2, 0 );
	view = reinterpret( x, 0 );

	// Row 0 divided by -1: (1,2)/(-1) = (-1,-2), (5,6)/(-1) = (-5,-6)
	assert.strictEqual( view[ 0 ], -1.0 );
	assert.strictEqual( view[ 1 ], -2.0 );

	// Row 1 divided by 0: produces Inf/NaN
	assert.ok( !isFinite( view[ 2 ] ) );
	assert.ok( !isFinite( view[ 3 ] ) );
	assert.strictEqual( view[ 4 ], -5.0 );
	assert.strictEqual( view[ 5 ], -6.0 );
	assert.ok( !isFinite( view[ 6 ] ) );
	assert.ok( !isFinite( view[ 7 ] ) );
});

test( 'zlarscl2: LDX > M (leading dimension larger than rows)', function t() {
	var view;
	var tc = ldx_gt_m;

	// X is 4-by-3 in memory but we only scale rows 0..1
	var x = new Complex128Array( [
		1, 0.1, 2, 0.2, 99, 99, 99, 99,
		3, 0.3, 4, 0.4, 99, 99, 99, 99,
		5, 0.5, 6, 0.6, 99, 99, 99, 99
	] );
	var d = new Float64Array( [ 2, 3 ] );
	zlarscl2( 2, 3, d, 1, 0, x, 1, 4, 0 );
	view = reinterpret( x, 0 );
	assertArrayClose( Array.from( view ), tc.x, 1e-14, 'x' );
});

test( 'zlarscl2: returns the output array X', function t() {
	var out;
	var x = new Complex128Array( [ 1, 2, 3, 4 ] );
	var d = new Float64Array( [ 2, 3 ] );
	out = zlarscl2( 2, 1, d, 1, 0, x, 1, 2, 0 );
	assert.strictEqual( out, x );
});

test( 'zlarscl2: supports non-unit stride for D', function t() {
	var view;

	// d = [2, ?, 3] with strideD=2 uses d[0]=2, d[2]=3
	var x = new Complex128Array( [
		1, 10, 2, 20,
		3, 30, 4, 40
	] );
	var d = new Float64Array( [ 2, 999, 3 ] );
	zlarscl2( 2, 2, d, 2, 0, x, 1, 2, 0 );
	view = reinterpret( x, 0 );

	// row 0 divided by 2: (1,10)/2=(0.5,5), (3,30)/2=(1.5,15)
	// row 1 divided by 3: (2,20)/3=(2/3,20/3), (4,40)/3=(4/3,40/3)
	assert.strictEqual( view[ 0 ], 0.5 );
	assert.strictEqual( view[ 1 ], 5.0 );
	assert.ok( Math.abs( view[ 2 ] - (2 / 3) ) < 1e-14 );
	assert.ok( Math.abs( view[ 3 ] - (20 / 3) ) < 1e-14 );
	assert.strictEqual( view[ 4 ], 1.5 );
	assert.strictEqual( view[ 5 ], 15.0 );
	assert.ok( Math.abs( view[ 6 ] - (4 / 3) ) < 1e-14 );
	assert.ok( Math.abs( view[ 7 ] - (40 / 3) ) < 1e-14 );
});

test( 'zlarscl2: supports offset for D', function t() {
	var view;

	// d starts at offset 1: d[1]=2, d[2]=3
	var x = new Complex128Array( [
		1, 10, 2, 20,
		3, 30, 4, 40
	] );
	var d = new Float64Array( [ 999, 2, 3 ] );
	zlarscl2( 2, 2, d, 1, 1, x, 1, 2, 0 );
	view = reinterpret( x, 0 );

	// row 0 divided by 2: (1,10)/2=(0.5,5), (3,30)/2=(1.5,15)
	// row 1 divided by 3: (2,20)/3=(2/3,20/3), (4,40)/3=(4/3,40/3)
	assert.strictEqual( view[ 0 ], 0.5 );
	assert.strictEqual( view[ 1 ], 5.0 );
	assert.ok( Math.abs( view[ 2 ] - (2 / 3) ) < 1e-14 );
	assert.ok( Math.abs( view[ 3 ] - (20 / 3) ) < 1e-14 );
	assert.strictEqual( view[ 4 ], 1.5 );
	assert.strictEqual( view[ 5 ], 15.0 );
	assert.ok( Math.abs( view[ 6 ] - (4 / 3) ) < 1e-14 );
	assert.ok( Math.abs( view[ 7 ] - (40 / 3) ) < 1e-14 );
});

test( 'zlarscl2: supports offset for X', function t() {
	var view;

	// X starts at offset 2 (complex elements)
	var x = new Complex128Array( [
		999, 999, 999, 999,
		1, 10, 2, 20,
		3, 30, 4, 40
	] );
	var d = new Float64Array( [ 2, 3 ] );
	zlarscl2( 2, 2, d, 1, 0, x, 1, 2, 2 );
	view = reinterpret( x, 0 );

	assert.strictEqual( view[ 0 ], 999 );
	assert.strictEqual( view[ 1 ], 999 );
	assert.strictEqual( view[ 2 ], 999 );
	assert.strictEqual( view[ 3 ], 999 );
	assert.strictEqual( view[ 4 ], 0.5 );
	assert.strictEqual( view[ 5 ], 5.0 );
	assert.ok( Math.abs( view[ 6 ] - (2 / 3) ) < 1e-14 );
	assert.ok( Math.abs( view[ 7 ] - (20 / 3) ) < 1e-14 );
	assert.strictEqual( view[ 8 ], 1.5 );
	assert.strictEqual( view[ 9 ], 15.0 );
	assert.ok( Math.abs( view[ 10 ] - (4 / 3) ) < 1e-14 );
	assert.ok( Math.abs( view[ 11 ] - (40 / 3) ) < 1e-14 );
});
