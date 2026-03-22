'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlaswp = require( './../lib' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlaswp.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assert.equal( actual[i], expected[i], msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zlaswp: main export is a function', function t() {
	assert.strictEqual( typeof zlaswp, 'function' );
});

test( 'zlaswp: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlaswp.ndarray, 'function' );
});

// Note: base.js uses 0-based k1/k2 and 0-based IPIV values.
// Fortran uses 1-based, so we convert in the test inputs.

test( 'zlaswp.ndarray performs forward row interchanges', function t() {
	var tc = findCase( 'basic_forward' );
	// A = [(1+2i) (7+8i); (3+4i) (9+10i); (5+6i) (11+12i)] col-major (3x2)
	// Fortran: ipiv=[3,2] k1=1 k2=2 => 0-based: ipiv=[2,1] k1=0 k2=1
	var a = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var ipiv = new Int32Array( [ 2, 1 ] );
	zlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 'basic_forward' );
});

test( 'zlaswp.ndarray is a no-op when ipiv(k) == k', function t() {
	var tc = findCase( 'no_swap' );
	var a = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var ipiv = new Int32Array( [ 0, 1 ] );
	zlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 'no_swap' );
});

test( 'zlaswp.ndarray performs reverse row interchanges (incx=-1)', function t() {
	var tc = findCase( 'reverse_pivots' );
	// Same matrix, Fortran ipiv=[3,2] with incx=-1
	// 0-based: ipiv=[2,1], k1=1 k2=0, incx=-1
	var a = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var ipiv = new Int32Array( [ 2, 1 ] );
	zlaswp.ndarray( 2, a, 1, 3, 0, 1, 0, ipiv, 1, 0, -1 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 'reverse_pivots' );
});

test( 'zlaswp.ndarray is a no-op when incx=0', function t() {
	var tc = findCase( 'incx_zero' );
	var a = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var ipiv = new Int32Array( [ 2 ] );
	zlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 0 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 'incx_zero' );
});

test( 'zlaswp.ndarray applies sequential swaps', function t() {
	var tc = findCase( 'two_swaps' );
	// 3x1 complex matrix: (10+1i), (20+2i), (30+3i)
	// Fortran: ipiv=[2,3] k1=1 k2=2 => 0-based: ipiv=[1,2] k1=0 k2=1
	var a = new Complex128Array( [ 10, 1, 20, 2, 30, 3 ] );
	var ipiv = new Int32Array( [ 1, 2 ] );
	zlaswp.ndarray( 1, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 'two_swaps' );
});

test( 'zlaswp.ndarray exercises block-tiled path (N=40 > 32 columns)', function t() {
	var tc = findCase( 'block_tiled' );
	// 3x40 complex matrix, col-major: a(i) = cmplx(i, i+0.5) for i=1..120
	var data = [];
	var i;
	for ( i = 1; i <= 120; i++ ) {
		data.push( i );
		data.push( i + 0.5 );
	}
	var a = new Complex128Array( data );
	// Fortran: ipiv=[3,2] k1=1 k2=2 => 0-based: ipiv=[2,1] k1=0 k2=1
	var ipiv = new Int32Array( [ 2, 1 ] );
	zlaswp.ndarray( 40, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	var view = reinterpret( a, 0 );
	assertArrayClose( Array.from( view ), tc.a, 'block_tiled' );
});

test( 'zlaswp.ndarray returns the input array', function t() {
	var a = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var ipiv = new Int32Array( [ 2, 1 ] );
	var result = zlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assert.strictEqual( result, a );
});

test( 'zlaswp.ndarray N=0 is a no-op', function t() {
	var a = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	var ipiv = new Int32Array( [ 2 ] );
	zlaswp.ndarray( 0, a, 1, 3, 0, 0, 0, ipiv, 1, 0, 1 );
	var view = reinterpret( a, 0 );
	// Should be unchanged since N=0
	assert.equal( view[0], 1 );
	assert.equal( view[1], 2 );
});
