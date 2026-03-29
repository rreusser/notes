

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zspmv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zspmv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// 3x3 complex symmetric matrix (packed upper):
// A = [2+i    1+i   3-2i]
//     [1+i    4-i   2+i ]
//     [3-2i   2+i   5+3i]
function upperAP() {
	return new Complex128Array( [ 2, 1, 1, 1, 4, -1, 3, -2, 2, 1, 5, 3 ] );
}

// Same matrix packed lower:
function lowerAP() {
	return new Complex128Array( [ 2, 1, 1, 1, 3, -2, 4, -1, 2, 1, 5, 3 ] );
}

function stdX() {
	return new Complex128Array( [ 1, 0.5, 2, -1, 3, 1 ] );
}


// TESTS //

test( 'zspmv is a function', function t() {
	assert.equal( typeof zspmv, 'function' );
});

test( 'zspmv: upper_basic', function t() {
	var tc = findCase( 'upper_basic' );
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( 3 );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: lower_basic', function t() {
	var tc = findCase( 'lower_basic' );
	var ap = lowerAP();
	var x = stdX();
	var y = new Complex128Array( 3 );
	var result = zspmv( 'lower', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: complex_alpha_beta', function t() {
	var tc = findCase( 'complex_alpha_beta' );
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 1, 2, -1, 0.5, 0.5 ] );
	var result = zspmv( 'upper', 3, new Complex128( 2, 1 ), ap, 1, 0, x, 1, 0, new Complex128( 0.5, -0.5 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var result = zspmv( 'upper', 3, new Complex128( 0, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 2, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 99, 0 ] );
	var result = zspmv( 'upper', 0, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: alpha_zero_beta_zero', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 99, 88, 77, 66, 55, 44 ] );
	var result = zspmv( 'upper', 3, new Complex128( 0, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: stride_2', function t() {
	var tc = findCase( 'stride_2' );
	var ap = upperAP();
	var x = new Complex128Array( [ 1, 0.5, 0, 0, 2, -1, 0, 0, 3, 1, 0, 0 ] );
	var y = new Complex128Array( 6 );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 2, 0, new Complex128( 0, 0 ), y, 2, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: scalar', function t() {
	var tc = findCase( 'scalar' );
	var ap = new Complex128Array( [ 3, 2 ] );
	var x = new Complex128Array( [ 5, 2 ] );
	var y = new Complex128Array( 1 );
	var result = zspmv( 'upper', 1, new Complex128( 2, 1 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: lower_nonzero_beta', function t() {
	var tc = findCase( 'lower_nonzero_beta' );
	var ap = lowerAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 1, 2, -1, 0.5, 0.5 ] );
	var result = zspmv( 'lower', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0.5, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: negative_incx', function t() {
	var tc = findCase( 'negative_incx' );
	// Fortran: x(1)=3+i, x(2)=2-i, x(3)=1+0.5i, incx=-1
	// With negative stride, start from end: elements are reversed
	var ap = upperAP();
	var x = new Complex128Array( [ 3, 1, 2, -1, 1, 0.5 ] );
	var y = new Complex128Array( 3 );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, -1, 2, new Complex128( 0, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: beta_one', function t() {
	var tc = findCase( 'beta_one' );
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 2, 3, -1, 2, 1 ] );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 1, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: lower_stride2_complex_beta', function t() {
	var tc = findCase( 'lower_stride2_complex_beta' );
	var ap = lowerAP();
	var x = new Complex128Array( [ 1, 0.5, 0, 0, 2, -1, 0, 0, 3, 1, 0, 0 ] );
	var y = new Complex128Array( [ 1, 1, 0, 0, 2, -1, 0, 0, 0.5, 0.5, 0, 0 ] );
	var result = zspmv( 'lower', 3, new Complex128( 2, 1 ), ap, 1, 0, x, 2, 0, new Complex128( 0.5, -0.5 ), y, 2, 0 );
	var view = reinterpret( result, 0 );
	assertArrayClose( Array.from( view ), tc.y, 1e-14, 'y' );
});

test( 'zspmv: alpha_zero_beta_one quick return', function t() {
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var result = zspmv( 'upper', 3, new Complex128( 0, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 1, 0 ), y, 1, 0 );
	var view = reinterpret( result, 0 );
	// y should be unchanged
	assertArrayClose( Array.from( view ), [ 1, 2, 3, 4, 5, 6 ], 1e-14, 'y' );
});

test( 'zspmv: returns y', function t() {
	var ap = upperAP();
	var x = stdX();
	var y = new Complex128Array( 3 );
	var result = zspmv( 'upper', 3, new Complex128( 1, 0 ), ap, 1, 0, x, 1, 0, new Complex128( 0, 0 ), y, 1, 0 );
	assert.equal( result, y );
});
