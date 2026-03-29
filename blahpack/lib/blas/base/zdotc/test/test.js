/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var real = require( '@stdlib/complex/float64/real' );
var imag = require( '@stdlib/complex/float64/imag' );
var zdotc = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zdotc.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that a Complex128 result matches expected [real, imag] from fixture.
*/
function assertComplexClose( result, expected, tol, msg ) {
	assertClose( real( result ), expected[ 0 ], tol, msg + ' real' );
	assertClose( imag( result ), expected[ 1 ], tol, msg + ' imag' );
}


// TESTS //

test( 'zdotc: main export is a function', function t() {
	assert.strictEqual( typeof zdotc, 'function' );
});

test( 'zdotc: basic (N=3, unit stride)', function t() {
	var result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	var tc = findCase( 'basic' );
	var x = new Complex128Array( tc.x );
	var y = new Complex128Array( tc.y );
	assertComplexClose( result, tc.result, 1e-14, 'basic' );
});

test( 'zdotc: N=0 returns (0,0)', function t() {
	var result = zdotc( 0, x, 1, 0, y, 1, 0 );
	var x = new Complex128Array( [ 1, 2, 3, 4 ] );
	var y = new Complex128Array( [ 5, 6, 7, 8 ] );
	assert.strictEqual( real( result ), 0.0 );
	assert.strictEqual( imag( result ), 0.0 );
});

test( 'zdotc: N<0 returns (0,0)', function t() {
	var result = zdotc( -1, x, 1, 0, y, 1, 0 );
	var x = new Complex128Array( [ 1, 2, 3, 4 ] );
	var y = new Complex128Array( [ 5, 6, 7, 8 ] );
	assert.strictEqual( real( result ), 0.0 );
	assert.strictEqual( imag( result ), 0.0 );
});

test( 'zdotc: N=1', function t() {
	var result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	var tc = findCase( 'n_one' );
	var x = new Complex128Array( tc.x );
	var y = new Complex128Array( tc.y );
	assertComplexClose( result, tc.result, 1e-14, 'n_one' );
});

test( 'zdotc: conjugation verification', function t() {
	var result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	var tc = findCase( 'conjugation' );
	var x = new Complex128Array( tc.x );
	var y = new Complex128Array( tc.y );

	// Without conjugation, result would be (0, 0) for [(1,0),(0,1)] dot [(1,0),(0,1)]

	// With conjugation, result is (2, 0) — verifies conjugation is applied
	assertComplexClose( result, tc.result, 1e-14, 'conjugation' );
});

test( 'zdotc: non-unit stride (strideX=2, strideY=1)', function t() {
	var result = zdotc( tc.N, x, 2, 0, y, 1, 0 );
	var tc = findCase( 'non_unit_stride' );
	var x = new Complex128Array( tc.x );
	var y = new Complex128Array( tc.y );
	assertComplexClose( result, tc.result, 1e-14, 'non_unit_stride' );
});

test( 'zdotc: negative stride (strideX=-1)', function t() {
	var result = zdotc( tc.N, x, -1, 2, y, 1, 0 );
	var tc = findCase( 'negative_stride' );
	var x = new Complex128Array( tc.x );
	var y = new Complex128Array( tc.y );
	assertComplexClose( result, tc.result, 1e-14, 'negative_stride' );
});

test( 'zdotc: both negative strides', function t() {
	var result = zdotc( tc.N, x, -1, 2, y, -1, 2 );
	var tc = findCase( 'both_negative' );
	var x = new Complex128Array( tc.x );
	var y = new Complex128Array( tc.y );
	assertComplexClose( result, tc.result, 1e-14, 'both_negative' );
});

test( 'zdotc: purely imaginary vectors', function t() {
	var result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	var tc = findCase( 'purely_imaginary' );
	var x = new Complex128Array( tc.x );
	var y = new Complex128Array( tc.y );
	assertComplexClose( result, tc.result, 1e-14, 'purely_imaginary' );
});

test( 'zdotc: larger N (N=6)', function t() {
	var result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	var tc = findCase( 'larger_n' );
	var x = new Complex128Array( tc.x );
	var y = new Complex128Array( tc.y );
	assertComplexClose( result, tc.result, 1e-14, 'larger_n' );
});

test( 'zdotc: offsetX and offsetY', function t() {
	// x = [junk, (1,2), (3,4), (5,6)], offsetX=1
	// y = [junk, junk, (7,8), (9,10), (11,12)], offsetY=2
	// Same as basic test: conj([(1,2),(3,4),(5,6)]) . [(7,8),(9,10),(11,12)] = (217,-18)
	var result = zdotc( 3, x, 1, 1, y, 1, 2 );
	var x = new Complex128Array( [ 99, 99, 1, 2, 3, 4, 5, 6 ] );
	var y = new Complex128Array( [ 99, 99, 99, 99, 7, 8, 9, 10, 11, 12 ] );
	assertClose( real( result ), 217.0, 1e-14, 'offset real' );
	assertClose( imag( result ), -18.0, 1e-14, 'offset imag' );
});

test( 'zdotc: self dot product gives squared norm', function t() {
	// conj(x) . x = sum |x_i|^2 (real, non-negative)
	// x = [(1,2), (3,4)]: |x0|^2 = 1+4=5, |x1|^2 = 9+16=25 => (30, 0)
	var result = zdotc( 2, x, 1, 0, x, 1, 0 );
	var x = new Complex128Array( [ 1, 2, 3, 4 ] );
	assertClose( real( result ), 30.0, 1e-14, 'self-dot real' );
	assertClose( imag( result ), 0.0, 1e-14, 'self-dot imag' );
});
