/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var real = require( '@stdlib/complex/float64/real' );
var imag = require( '@stdlib/complex/float64/imag' );
var zdotu = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zdotu.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// TESTS //

test( 'zdotu: main export is a function', function t() {
	assert.strictEqual( typeof zdotu, 'function' );
});

test( 'zdotu: basic (N=3, unit stride)', function t() {
	var tc = findCase( 'basic' );
	var x = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var y = new Complex128Array( [ 7, 8, 9, 10, 11, 12 ] );
	var result = zdotu( 3, x, 1, 0, y, 1, 0 );
	assertClose( real( result ), tc.result[ 0 ], 1e-14, 'real' );
	assertClose( imag( result ), tc.result[ 1 ], 1e-14, 'imag' );
});

test( 'zdotu: N=0 returns (0,0)', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Complex128Array( [ 1, 2, 3, 4 ] );
	var y = new Complex128Array( [ 5, 6, 7, 8 ] );
	var result = zdotu( 0, x, 1, 0, y, 1, 0 );
	assertClose( real( result ), tc.result[ 0 ], 1e-14, 'real' );
	assertClose( imag( result ), tc.result[ 1 ], 1e-14, 'imag' );
});

test( 'zdotu: N=1', function t() {
	var tc = findCase( 'n_one' );
	var x = new Complex128Array( [ 3, 4 ] );
	var y = new Complex128Array( [ 1, 2 ] );
	var result = zdotu( 1, x, 1, 0, y, 1, 0 );
	assertClose( real( result ), tc.result[ 0 ], 1e-14, 'real' );
	assertClose( imag( result ), tc.result[ 1 ], 1e-14, 'imag' );
});

test( 'zdotu: non-unit stride (incx=2, incy=1)', function t() {
	var tc = findCase( 'non_unit_stride' );
	var x = new Complex128Array( [ 1, 2, 99, 99, 3, 4, 99, 99, 5, 6 ] );
	var y = new Complex128Array( [ 7, 8, 9, 10, 11, 12 ] );
	var result = zdotu( 3, x, 2, 0, y, 1, 0 );
	assertClose( real( result ), tc.result[ 0 ], 1e-14, 'real' );
	assertClose( imag( result ), tc.result[ 1 ], 1e-14, 'imag' );
});

test( 'zdotu: negative stride (incx=-1)', function t() {
	var tc = findCase( 'negative_stride' );
	// With incx=-1, Fortran starts at element N=3 and goes backward.
	// In our base.js, we pass offset = N-1 = 2, stride = -1.
	var x = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var y = new Complex128Array( [ 7, 8, 9, 10, 11, 12 ] );
	var result = zdotu( 3, x, -1, 2, y, 1, 0 );
	assertClose( real( result ), tc.result[ 0 ], 1e-14, 'real' );
	assertClose( imag( result ), tc.result[ 1 ], 1e-14, 'imag' );
});

test( 'zdotu: both negative strides', function t() {
	var tc = findCase( 'both_negative' );
	var x = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var y = new Complex128Array( [ 7, 8, 9, 10, 11, 12 ] );
	var result = zdotu( 3, x, -1, 2, y, -1, 2 );
	assertClose( real( result ), tc.result[ 0 ], 1e-14, 'real' );
	assertClose( imag( result ), tc.result[ 1 ], 1e-14, 'imag' );
});

test( 'zdotu: purely real vectors', function t() {
	var tc = findCase( 'purely_real' );
	var x = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );
	var y = new Complex128Array( [ 4, 0, 5, 0, 6, 0 ] );
	var result = zdotu( 3, x, 1, 0, y, 1, 0 );
	assertClose( real( result ), tc.result[ 0 ], 1e-14, 'real' );
	assertClose( imag( result ), tc.result[ 1 ], 1e-14, 'imag' );
});

test( 'zdotu: purely imaginary vectors', function t() {
	var tc = findCase( 'purely_imaginary' );
	var x = new Complex128Array( [ 0, 1, 0, 2, 0, 3 ] );
	var y = new Complex128Array( [ 0, 4, 0, 5, 0, 6 ] );
	var result = zdotu( 3, x, 1, 0, y, 1, 0 );
	assertClose( real( result ), tc.result[ 0 ], 1e-14, 'real' );
	assertClose( imag( result ), tc.result[ 1 ], 1e-14, 'imag' );
});

test( 'zdotu: larger N (N=6)', function t() {
	var tc = findCase( 'larger_n' );
	var x = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6 ] );
	var y = new Complex128Array( [ 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ] );
	var result = zdotu( 6, x, 1, 0, y, 1, 0 );
	assertClose( real( result ), tc.result[ 0 ], 1e-14, 'real' );
	assertClose( imag( result ), tc.result[ 1 ], 1e-14, 'imag' );
});

test( 'zdotu: offset support', function t() {
	// Use offset to skip first element: x starts at index 1, y starts at index 1
	var x = new Complex128Array( [ 99, 99, 3, 4 ] );
	var y = new Complex128Array( [ 88, 88, 1, 2 ] );
	// zdotu(1, x[1:], 1, y[1:], 1) = (3,4)*(1,2) = (3-8, 6+4) = (-5, 10)
	var result = zdotu( 1, x, 1, 1, y, 1, 1 );
	assertClose( real( result ), -5.0, 1e-14, 'real' );
	assertClose( imag( result ), 10.0, 1e-14, 'imag' );
});
