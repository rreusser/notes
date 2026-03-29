

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var path = require( 'path' );
var zlar2v = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlar2v.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zlar2v is a function', function t() {
	assert.equal( typeof zlar2v, 'function' );
});

test( 'zlar2v: basic (N=4, unit strides, mixed complex S)', function t() {
	var tc = findCase( 'basic' );
	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
	var y = new Complex128Array( [ 5.0, 0.0, 6.0, 0.0, 7.0, 0.0, 8.0, 0.0 ] );
	var z = new Complex128Array( [ 0.5, 0.1, 1.0, -0.2, 1.5, 0.3, 2.0, -0.4 ] );
	var c = new Float64Array( [ 0.8660254037844387, 0.7071067811865476, 0.5, 0.0 ] );
	var s = new Complex128Array( [ 0.5, 0.0, 0.5, 0.5, 0.0, 0.8660254037844387, 0.8, 0.6 ] );

	zlar2v( 4, x, 1, 0, y, 1, 0, z, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
	assertArrayClose( Array.from( reinterpret( z, 0 ) ), tc.z, 1e-14, 'z' );
});

test( 'zlar2v: n_zero (quick return)', function t() {
	var x = new Complex128Array( [ 1.0, 0.0 ] );
	var y = new Complex128Array( [ 2.0, 0.0 ] );
	var z = new Complex128Array( [ 0.5, 0.1 ] );
	var c = new Float64Array( [ 0.5 ] );
	var s = new Complex128Array( [ 0.8660254037844387, 0.0 ] );

	zlar2v( 0, x, 1, 0, y, 1, 0, z, 1, 0, c, 1, 0, s, 1, 0 );

	// Arrays unchanged
	var xv = reinterpret( x, 0 );
	var yv = reinterpret( y, 0 );
	var zv = reinterpret( z, 0 );
	assertClose( xv[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( xv[ 1 ], 0.0, 1e-14, 'x[1]' );
	assertClose( yv[ 0 ], 2.0, 1e-14, 'y[0]' );
	assertClose( yv[ 1 ], 0.0, 1e-14, 'y[1]' );
	assertClose( zv[ 0 ], 0.5, 1e-14, 'z[0]' );
	assertClose( zv[ 1 ], 0.1, 1e-14, 'z[1]' );
});

test( 'zlar2v: n_one (single element)', function t() {
	var tc = findCase( 'n_one' );
	var x = new Complex128Array( [ 3.0, 0.0 ] );
	var y = new Complex128Array( [ 4.0, 0.0 ] );
	var z = new Complex128Array( [ 1.0, 0.5 ] );
	var c = new Float64Array( [ 0.6 ] );
	var s = new Complex128Array( [ 0.8, 0.0 ] );

	zlar2v( 1, x, 1, 0, y, 1, 0, z, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
	assertArrayClose( Array.from( reinterpret( z, 0 ) ), tc.z, 1e-14, 'z' );
});

test( 'zlar2v: non_unit_stride (INCX=2, INCC=2)', function t() {
	var tc = findCase( 'non_unit_stride' );
	// N=3, INCX=2, INCC=2
	// X at positions 0, 2, 4 (complex elements); Y same; Z same
	var x = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 3.0, 0.0 ] );
	var y = new Complex128Array( [ 4.0, 0.0, 0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 6.0, 0.0 ] );
	var z = new Complex128Array( [ 0.5, 0.2, 0.0, 0.0, 1.0, -0.3, 0.0, 0.0, 1.5, 0.4 ] );
	var c = new Float64Array( [ 0.8, 0.0, 0.6, 0.0, 0.5 ] );
	var s = new Complex128Array( [ 0.6, 0.0, 0.0, 0.0, 0.0, 0.8, 0.0, 0.0, 0.5, 0.5 ] );

	zlar2v( 3, x, 2, 0, y, 2, 0, z, 2, 0, c, 2, 0, s, 2, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
	assertArrayClose( Array.from( reinterpret( z, 0 ) ), tc.z, 1e-14, 'z' );
});

test( 'zlar2v: identity rotation (c=1, s=0)', function t() {
	var tc = findCase( 'identity' );
	var x = new Complex128Array( [ 10.0, 0.0, 20.0, 0.0, 30.0, 0.0 ] );
	var y = new Complex128Array( [ 40.0, 0.0, 50.0, 0.0, 60.0, 0.0 ] );
	var z = new Complex128Array( [ 5.0, 1.0, 10.0, -2.0, 15.0, 3.0 ] );
	var c = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	var s = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );

	zlar2v( 3, x, 1, 0, y, 1, 0, z, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
	assertArrayClose( Array.from( reinterpret( z, 0 ) ), tc.z, 1e-14, 'z' );
});

test( 'zlar2v: pure imaginary S (c=0.6, s=(0,0.8))', function t() {
	var tc = findCase( 'pure_imag_s' );
	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
	var y = new Complex128Array( [ 3.0, 0.0, 4.0, 0.0 ] );
	var z = new Complex128Array( [ 0.5, 0.3, 1.0, -0.5 ] );
	var c = new Float64Array( [ 0.6, 0.6 ] );
	var s = new Complex128Array( [ 0.0, 0.8, 0.0, 0.8 ] );

	zlar2v( 2, x, 1, 0, y, 1, 0, z, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
	assertArrayClose( Array.from( reinterpret( z, 0 ) ), tc.z, 1e-14, 'z' );
});

test( 'zlar2v: swap rotation (c=0, s=(1,0))', function t() {
	var tc = findCase( 'swap' );
	var x = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
	var y = new Complex128Array( [ 3.0, 0.0, 4.0, 0.0 ] );
	var z = new Complex128Array( [ 0.5, 0.7, 1.0, -0.3 ] );
	var c = new Float64Array( [ 0.0, 0.0 ] );
	var s = new Complex128Array( [ 1.0, 0.0, 1.0, 0.0 ] );

	zlar2v( 2, x, 1, 0, y, 1, 0, z, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
	assertArrayClose( Array.from( reinterpret( z, 0 ) ), tc.z, 1e-14, 'z' );
});

test( 'zlar2v: mixed strides (INCX=3, INCC=2)', function t() {
	var tc = findCase( 'mixed_strides' );
	// N=2, INCX=3, INCC=2
	// Fortran X at positions 1,4 → complex elements 0,3
	var x = new Complex128Array( [ 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0 ] );
	var y = new Complex128Array( [ 6.0, 0.0, 0.0, 0.0, 0.0, 0.0, 8.0, 0.0 ] );
	var z = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 0.0, 0.0, 2.0, -1.0 ] );
	var c = new Float64Array( [ 0.8, 0.0, 0.6 ] );
	var s = new Complex128Array( [ 0.6, 0.0, 0.0, 0.0, 0.5, 0.5 ] );

	zlar2v( 2, x, 3, 0, y, 3, 0, z, 3, 0, c, 2, 0, s, 2, 0 );

	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 1e-14, 'y' );
	assertArrayClose( Array.from( reinterpret( z, 0 ) ), tc.z, 1e-14, 'z' );
});
