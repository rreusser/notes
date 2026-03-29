'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlartv = require( './../lib' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlartv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zlartv: main export is a function', function t() {
	assert.strictEqual( typeof zlartv, 'function' );
});

test( 'zlartv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlartv.ndarray, 'function' );
});

test( 'zlartv: basic', function t() {
	var tc = findCase( 'basic' );
	var x = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var y = new Complex128Array( [ 9, 10, 11, 12, 13, 14, 15, 16 ] );
	var c = new Float64Array( [ 0.8, 0.6, 0.5, 0.0 ] );
	var s = new Complex128Array( [ 0.6, 0.0, 0.0, 0.8, 0.5, 0.5, 1.0, 0.0 ] );

	zlartv.ndarray( 4, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zlartv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Complex128Array( [ 1, 2 ] );
	var y = new Complex128Array( [ 3, 4 ] );
	var c = new Float64Array( [ 0.5 ] );
	var s = new Complex128Array( [ 0.8, 0.1 ] );

	zlartv.ndarray( 0, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zlartv: n_one', function t() {
	var tc = findCase( 'n_one' );
	var x = new Complex128Array( [ 3, 4 ] );
	var y = new Complex128Array( [ 5, 6 ] );
	var c = new Float64Array( [ 0.6 ] );
	var s = new Complex128Array( [ 0.8, 0.0 ] );

	zlartv.ndarray( 1, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zlartv: non_unit_stride', function t() {
	var tc = findCase( 'non_unit_stride' );
	var x = new Complex128Array( [ 1, 2, 0, 0, 3, 4, 0, 0, 5, 6 ] );
	var y = new Complex128Array( [ 7, 8, 0, 0, 9, 10, 0, 0, 11, 12 ] );
	var c = new Float64Array( [ 0.8, 0, 0.6, 0, 0.5 ] );
	var s = new Complex128Array( [ 0.6, 0.1, 0, 0, 0.5, 0.3, 0, 0, 0.7, 0.2 ] );

	zlartv.ndarray( 3, x, 2, 0, y, 2, 0, c, 2, 0, s, 2, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zlartv: identity', function t() {
	var tc = findCase( 'identity' );
	var x = new Complex128Array( [ 10, 20, 30, 40, 50, 60 ] );
	var y = new Complex128Array( [ 70, 80, 90, 100, 110, 120 ] );
	var c = new Float64Array( [ 1, 1, 1 ] );
	var s = new Complex128Array( [ 0, 0, 0, 0, 0, 0 ] );

	zlartv.ndarray( 3, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zlartv: swap', function t() {
	var tc = findCase( 'swap' );
	var x = new Complex128Array( [ 1, 2, 3, 4 ] );
	var y = new Complex128Array( [ 5, 6, 7, 8 ] );
	var c = new Float64Array( [ 0, 0 ] );
	var s = new Complex128Array( [ 1, 0, 1, 0 ] );

	zlartv.ndarray( 2, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zlartv: imag_s', function t() {
	var tc = findCase( 'imag_s' );
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	var y = new Complex128Array( [ 0, 1, 1, 0 ] );
	var c = new Float64Array( [ 0, 0 ] );
	var s = new Complex128Array( [ 0, 1, 0, 1 ] );

	zlartv.ndarray( 2, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});

test( 'zlartv: mixed_strides', function t() {
	var tc = findCase( 'mixed_strides' );
	var x = new Complex128Array( [ 2, 1, 4, 3 ] );
	var y = new Complex128Array( [ 6, 5, 0, 0, 0, 0, 8, 7 ] );
	var c = new Float64Array( [ 0.8, 0, 0.6 ] );
	var s = new Complex128Array( [ 0.6, 0.1, 0, 0, 0.8, 0.2 ] );

	zlartv.ndarray( 2, x, 1, 0, y, 3, 0, c, 2, 0, s, 2, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
});
