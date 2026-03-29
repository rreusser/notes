/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgemv = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgemv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

// A = [1 4; 2 5; 3 6] column-major (3x2): strideA1=1, strideA2=3
var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );

test( 'dgemv: basic trans=N', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 1, 2 ] );
	var y = new Float64Array( 3 );
	dgemv( 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'basic' );
});

test( 'dgemv: transpose trans=T', function t() {
	var tc = findCase( 'transpose' );
	var x = new Float64Array( [ 1, 2, 3 ] );
	var y = new Float64Array( 2 );
	dgemv( 'transpose', 3, 2, 1.0, A, 1, 3, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'transpose' );
});

test( 'dgemv: alpha and beta scaling', function t() {
	var tc = findCase( 'alpha_beta' );
	var x = new Float64Array( [ 1, 2 ] );
	var y = new Float64Array( [ 10, 20, 30 ] );
	dgemv( 'no-transpose', 3, 2, 2.0, A, 1, 3, 0, x, 1, 0, 3.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'alpha_beta' );
});

test( 'dgemv: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var y = new Float64Array( [ 99 ] );
	dgemv( 'no-transpose', 3, 0, 1.0, A, 1, 3, 0, new Float64Array( 2 ), 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'n_zero' );
});

test( 'dgemv: M=0 quick return', function t() {
	var tc = findCase( 'm_zero' );
	var y = new Float64Array( [ 99 ] );
	dgemv( 'no-transpose', 0, 2, 1.0, A, 1, 1, 0, new Float64Array( 2 ), 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'm_zero' );
});

test( 'dgemv: non-unit strides incx=2, incy=2', function t() {
	var tc = findCase( 'stride' );
	var x = new Float64Array( 20 );
	x[ 0 ] = 1; x[ 2 ] = 2;
	var y = new Float64Array( 20 );
	y[ 0 ] = 10; y[ 2 ] = 20; y[ 4 ] = 30;
	dgemv( 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, x, 2, 0, 1.0, y, 2, 0 );
	assertArrayClose( y.subarray( 0, 6 ), tc.y, 1e-14, 'stride' );
});

test( 'dgemv: transpose with alpha and beta', function t() {
	var tc = findCase( 'transpose_alpha_beta' );
	var x = new Float64Array( [ 1, 1, 1 ] );
	var y = new Float64Array( [ 5, 10 ] );
	dgemv( 'transpose', 3, 2, 2.0, A, 1, 3, 0, x, 1, 0, 3.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'transpose_alpha_beta' );
});

test( 'dgemv: alpha=0 just scales y by beta', function t() {
	var tc = findCase( 'alpha_zero' );
	var y = new Float64Array( [ 10, 20, 30 ] );
	dgemv( 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, new Float64Array( 2 ), 1, 0, 2.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'alpha_zero' );
});

// ndarray validation tests

test( 'dgemv: ndarray throws TypeError for invalid trans', function t() {
	assert.throws( function () {
		ndarray( 'invalid', 2, 2, 1.0, A, 1, 3, 0, new Float64Array( 2 ), 1, 0, 0.0, new Float64Array( 3 ), 1, 0 );
	}, TypeError );
});

test( 'dgemv: ndarray throws RangeError for negative M', function t() {
	assert.throws( function () {
		ndarray( 'no-transpose', -1, 2, 1.0, A, 1, 3, 0, new Float64Array( 2 ), 1, 0, 0.0, new Float64Array( 3 ), 1, 0 );
	}, RangeError );
});

test( 'dgemv: ndarray throws RangeError for negative N', function t() {
	assert.throws( function () {
		ndarray( 'no-transpose', 2, -1, 1.0, A, 1, 3, 0, new Float64Array( 2 ), 1, 0, 0.0, new Float64Array( 3 ), 1, 0 );
	}, RangeError );
});

test( 'dgemv: ndarray throws RangeError for zero strideX', function t() {
	assert.throws( function () {
		ndarray( 'no-transpose', 2, 2, 1.0, A, 1, 3, 0, new Float64Array( 2 ), 0, 0, 0.0, new Float64Array( 3 ), 1, 0 );
	}, RangeError );
});

test( 'dgemv: ndarray throws RangeError for zero strideY', function t() {
	assert.throws( function () {
		ndarray( 'no-transpose', 2, 2, 1.0, A, 1, 3, 0, new Float64Array( 2 ), 1, 0, 0.0, new Float64Array( 3 ), 0, 0 );
	}, RangeError );
});
