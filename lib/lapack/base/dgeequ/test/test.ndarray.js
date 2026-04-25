'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgeequ = require( './../lib/ndarray.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var diagonal_varied = require( './fixtures/diagonal_varied.json' );
var zero_row = require( './fixtures/zero_row.json' );
var zero_col = require( './fixtures/zero_col.json' );
var identity = require( './fixtures/identity.json' );
var m_zero = require( './fixtures/m_zero.json' );
var n_zero = require( './fixtures/n_zero.json' );
var nonsquare = require( './fixtures/nonsquare.json' );

// FUNCTIONS //

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

test( 'dgeequ: basic 3x3 well-conditioned matrix', function t() {
	var tc = basic;
	var A = new Float64Array( [ 4.0, 1.0, 0.5, 1.0, 3.0, 1.0, 0.5, 1.0, 2.0 ] );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: diagonal matrix with varying scales', function t() {
	var tc = diagonal_varied;
	var A = new Float64Array( 9 );
	A[ 0 ] = 100.0; A[ 4 ] = 1.0; A[ 8 ] = 0.01;
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: matrix with zero row returns info=i', function t() {
	var tc = zero_row;
	var A = new Float64Array( [ 1.0, 0.0, 1.0, 2.0, 0.0, 3.0, 4.0, 0.0, 5.0 ] );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
});

test( 'dgeequ: matrix with zero column returns info=M+j', function t() {
	var tc = zero_col;
	var A = new Float64Array( [ 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 4.0, 5.0, 6.0 ] );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
});

test( 'dgeequ: identity matrix', function t() {
	var tc = identity;
	var A = new Float64Array( 9 );
	A[ 0 ] = 1.0; A[ 4 ] = 1.0; A[ 8 ] = 1.0;
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: quick return M=0', function t() {
	var tc = m_zero;
	var r = new Float64Array( 0 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 0, 3, new Float64Array( 0 ), 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, 0 );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: quick return N=0', function t() {
	var tc = n_zero;
	var r = new Float64Array( 3 );
	var c = new Float64Array( 0 );
	var result = dgeequ( 3, 0, new Float64Array( 0 ), 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, 0 );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: non-square 2x4 matrix', function t() {
	var tc = nonsquare;
	var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
	var r = new Float64Array( 2 );
	var c = new Float64Array( 4 );
	var result = dgeequ( 2, 4, A, 1, 2, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});
