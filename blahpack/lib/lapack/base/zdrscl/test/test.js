'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zdrscl = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zdrscl.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zdrscl: main export is a function', function t() {
	assert.strictEqual( typeof zdrscl, 'function' );
});

test( 'zdrscl: basic scaling by 1/2 (sa=2)', function t() {
	var tc = findCase( 'basic_scale_2' );
	var x = new Complex128Array( [ 2.0, 4.0, 6.0, 8.0, 10.0, 12.0 ] );
	zdrscl( 3, 2.0, x, 1, 0 );
	assertArrayClose( reinterpret( x, 0 ), tc.x, 1e-14, 'x' );
});

test( 'zdrscl: identity scale (sa=1)', function t() {
	var tc = findCase( 'identity_scale' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	zdrscl( 3, 1.0, x, 1, 0 );
	assertArrayClose( reinterpret( x, 0 ), tc.x, 1e-14, 'x' );
});

test( 'zdrscl: scale by 1/0.5 (multiply by 2)', function t() {
	var tc = findCase( 'scale_half' );
	var x = new Complex128Array( [ 1.0, 3.0, 5.0, 7.0 ] );
	zdrscl( 2, 0.5, x, 1, 0 );
	assertArrayClose( reinterpret( x, 0 ), tc.x, 1e-14, 'x' );
});

test( 'zdrscl: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Complex128Array( [ 99.0, 88.0 ] );
	zdrscl( 0, 2.0, x, 1, 0 );
	assertArrayClose( reinterpret( x, 0 ), tc.x, 1e-14, 'x' );
});

test( 'zdrscl: N=1', function t() {
	var tc = findCase( 'n_one' );
	var x = new Complex128Array( [ 4.0, -6.0 ] );
	zdrscl( 1, 2.0, x, 1, 0 );
	assertArrayClose( reinterpret( x, 0 ), tc.x, 1e-14, 'x' );
});

test( 'zdrscl: large scalar (overflow protection)', function t() {
	var tc = findCase( 'large_scalar' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zdrscl( 2, 1.0e300, x, 1, 0 );
	assertArrayClose( reinterpret( x, 0 ), tc.x, 1e-10, 'x' );
});

test( 'zdrscl: small scalar (underflow protection)', function t() {
	var tc = findCase( 'small_scalar' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zdrscl( 2, 1.0e-300, x, 1, 0 );
	assertArrayClose( reinterpret( x, 0 ), tc.x, 1e-10, 'x' );
});

test( 'zdrscl: non-unit stride (incx=2)', function t() {
	var tc = findCase( 'stride_2' );
	var x = new Complex128Array( [ 1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0 ] );
	zdrscl( 2, 4.0, x, 2, 0 );
	assertArrayClose( reinterpret( x, 0 ), tc.x, 1e-14, 'x' );
});
