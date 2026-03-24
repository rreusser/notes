'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgttrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgttrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgttrf: basic_5x5', function t() {
	var tc = findCase( 'basic_5x5' );
	var dl = new Float64Array( [ -1.0, -1.0, -1.0, -1.0 ] );
	var d = new Float64Array( [ 2.0, 2.0, 2.0, 2.0, 2.0 ] );
	var du = new Float64Array( [ -1.0, -1.0, -1.0, -1.0 ] );
	var du2 = new Float64Array( 3 );
	var ipiv = new Int32Array( 5 );
	var info;

	info = dgttrf( 5, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( dl, new Float64Array( tc.dl ), 1e-14, 'dl' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( du, new Float64Array( tc.du ), 1e-14, 'du' );
	assertArrayClose( du2, new Float64Array( tc.du2 ), 1e-14, 'du2' );
	// Fortran IPIV is 1-based; JS is 0-based
	assert.deepEqual( Array.from( ipiv ), tc.ipiv.map( function( v ) { return v - 1; } ), 'ipiv' );
});

test( 'dgttrf: n_one', function t() {
	var tc = findCase( 'n_one' );
	var dl = new Float64Array( 0 );
	var d = new Float64Array( [ 5.0 ] );
	var du = new Float64Array( 0 );
	var du2 = new Float64Array( 0 );
	var ipiv = new Int32Array( 1 );
	var info;

	info = dgttrf( 1, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assert.deepEqual( Array.from( ipiv ), tc.ipiv.map( function( v ) { return v - 1; } ), 'ipiv' );
});

test( 'dgttrf: n_two', function t() {
	var tc = findCase( 'n_two' );
	var dl = new Float64Array( [ 3.0 ] );
	var d = new Float64Array( [ 4.0, 7.0 ] );
	var du = new Float64Array( [ 1.0 ] );
	var du2 = new Float64Array( 0 );
	var ipiv = new Int32Array( 2 );
	var info;

	info = dgttrf( 2, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( dl, new Float64Array( tc.dl ), 1e-14, 'dl' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( du, new Float64Array( tc.du ), 1e-14, 'du' );
	assert.deepEqual( Array.from( ipiv ), tc.ipiv.map( function( v ) { return v - 1; } ), 'ipiv' );
});

test( 'dgttrf: n_two_pivot', function t() {
	var tc = findCase( 'n_two_pivot' );
	var dl = new Float64Array( [ 5.0 ] );
	var d = new Float64Array( [ 2.0, 3.0 ] );
	var du = new Float64Array( [ 1.0 ] );
	var du2 = new Float64Array( 0 );
	var ipiv = new Int32Array( 2 );
	var info;

	info = dgttrf( 2, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( dl, new Float64Array( tc.dl ), 1e-14, 'dl' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( du, new Float64Array( tc.du ), 1e-14, 'du' );
	assert.deepEqual( Array.from( ipiv ), tc.ipiv.map( function( v ) { return v - 1; } ), 'ipiv' );
});

test( 'dgttrf: singular', function t() {
	var tc = findCase( 'singular' );
	var dl = new Float64Array( [ 1.0, 1.0 ] );
	var d = new Float64Array( [ 0.0, 0.0, 1.0 ] );
	var du = new Float64Array( [ 0.0, 1.0 ] );
	var du2 = new Float64Array( 1 );
	var ipiv = new Int32Array( 3 );
	var info;

	info = dgttrf( 3, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );

	assert.equal( info, tc.info, 'info (singular, 1-based)' );
	assertArrayClose( dl, new Float64Array( tc.dl ), 1e-14, 'dl' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( du, new Float64Array( tc.du ), 1e-14, 'du' );
	assertArrayClose( du2, new Float64Array( tc.du2 ), 1e-14, 'du2' );
	assert.deepEqual( Array.from( ipiv ), tc.ipiv.map( function( v ) { return v - 1; } ), 'ipiv' );
});

test( 'dgttrf: n_zero', function t() {
	var dl = new Float64Array( 0 );
	var d = new Float64Array( 0 );
	var du = new Float64Array( 0 );
	var du2 = new Float64Array( 0 );
	var ipiv = new Int32Array( 0 );
	var info;

	info = dgttrf( 0, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );

	assert.equal( info, 0, 'info' );
});

test( 'dgttrf: pivot_5x5', function t() {
	var tc = findCase( 'pivot_5x5' );
	var dl = new Float64Array( [ 10.0, 10.0, 10.0, 10.0 ] );
	var d = new Float64Array( [ 1.0, 1.0, 1.0, 1.0, 1.0 ] );
	var du = new Float64Array( [ 2.0, 2.0, 2.0, 2.0 ] );
	var du2 = new Float64Array( 3 );
	var ipiv = new Int32Array( 5 );
	var info;

	info = dgttrf( 5, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( dl, new Float64Array( tc.dl ), 1e-14, 'dl' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( du, new Float64Array( tc.du ), 1e-14, 'du' );
	assertArrayClose( du2, new Float64Array( tc.du2 ), 1e-14, 'du2' );
	assert.deepEqual( Array.from( ipiv ), tc.ipiv.map( function( v ) { return v - 1; } ), 'ipiv' );
});

test( 'dgttrf: negative N returns -1', function t() {
	var dl = new Float64Array( 0 );
	var d = new Float64Array( 0 );
	var du = new Float64Array( 0 );
	var du2 = new Float64Array( 0 );
	var ipiv = new Int32Array( 0 );
	var info;

	info = dgttrf( -1, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );

	assert.equal( info, -1, 'info' );
});

test( 'dgttrf: supports stride and offset parameters', function t() {
	// Embed the N=2 case inside larger arrays with stride=2 and offset=1
	var dl = new Float64Array( [ 0.0, 3.0, 0.0 ] );
	var d = new Float64Array( [ 0.0, 4.0, 0.0, 7.0, 0.0 ] );
	var du = new Float64Array( [ 0.0, 1.0, 0.0 ] );
	var du2 = new Float64Array( 1 );
	var ipiv = new Int32Array( [ 0, 0, 0, 0, 0 ] );
	var info;
	var tc;

	tc = findCase( 'n_two' );

	info = dgttrf( 2, dl, 2, 1, d, 2, 1, du, 2, 1, du2, 1, 0, ipiv, 2, 1 );

	assert.equal( info, tc.info, 'info' );
	assertClose( dl[ 1 ], tc.dl[ 0 ], 1e-14, 'dl[0]' );
	assertClose( d[ 1 ], tc.d[ 0 ], 1e-14, 'd[0]' );
	assertClose( d[ 3 ], tc.d[ 1 ], 1e-14, 'd[1]' );
	assertClose( du[ 1 ], tc.du[ 0 ], 1e-14, 'du[0]' );
	assert.equal( ipiv[ 1 ], tc.ipiv[ 0 ] - 1, 'ipiv[0]' );
	assert.equal( ipiv[ 3 ], tc.ipiv[ 1 ] - 1, 'ipiv[1]' );
});
