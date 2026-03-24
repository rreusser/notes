

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgtsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgtsv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Extracts a subarray from a Float64Array.
*/
function toArray( arr, offset, length ) {
	var out = [];
	var i;
	for ( i = 0; i < length; i++ ) {
		out.push( arr[ offset + i ] );
	}
	return out;
}

/**
* Extracts a column from a column-major matrix stored in a Float64Array.
*/
function getColumn( B, LDB, col, N ) {
	var out = [];
	var i;
	for ( i = 0; i < N; i++ ) {
		out.push( B[ col * LDB + i ] );
	}
	return out;
}


// TESTS //

test( 'dgtsv: basic_5x5_single_rhs', function t() {
	var tc = findCase( 'basic_5x5_single_rhs' );
	var N = 5;
	var dl = new Float64Array( [ -1.0, -1.0, -1.0, -1.0 ] );
	var d = new Float64Array( [ 2.0, 2.0, 2.0, 2.0, 2.0 ] );
	var du = new Float64Array( [ -1.0, -1.0, -1.0, -1.0 ] );
	var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );

	var info = dgtsv( N, 1, dl, 1, 0, d, 1, 0, du, 1, 0, B, 1, N, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( toArray( B, 0, N ), tc.b, 1e-14, 'b' );
	assertArrayClose( toArray( d, 0, N ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( dl, 0, N - 1 ), tc.dl, 1e-14, 'dl' );
	assertArrayClose( toArray( du, 0, N - 1 ), tc.du, 1e-14, 'du' );
});

test( 'dgtsv: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var N = 4;
	var nrhs = 2;
	var LDB = N;
	var dl = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	var d = new Float64Array( [ 3.0, 3.0, 3.0, 3.0 ] );
	var du = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	// Column-major: B(:,1) then B(:,2)
	var B = new Float64Array( [
		1.0, 0.0, 0.0, 1.0,
		2.0, 1.0, 1.0, 2.0
	] );

	var info = dgtsv( N, nrhs, dl, 1, 0, d, 1, 0, du, 1, 0, B, 1, LDB, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( getColumn( B, LDB, 0, N ), tc.b1, 1e-14, 'b1' );
	assertArrayClose( getColumn( B, LDB, 1, N ), tc.b2, 1e-14, 'b2' );
	assertArrayClose( toArray( d, 0, N ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( dl, 0, N - 1 ), tc.dl, 1e-14, 'dl' );
	assertArrayClose( toArray( du, 0, N - 1 ), tc.du, 1e-14, 'du' );
});

test( 'dgtsv: n_one', function t() {
	var tc = findCase( 'n_one' );
	var d = new Float64Array( [ 5.0 ] );
	var dl = new Float64Array( 0 );
	var du = new Float64Array( 0 );
	var B = new Float64Array( [ 10.0 ] );

	var info = dgtsv( 1, 1, dl, 1, 0, d, 1, 0, du, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( toArray( B, 0, 1 ), tc.b, 1e-14, 'b' );
	assertArrayClose( toArray( d, 0, 1 ), tc.d, 1e-14, 'd' );
});

test( 'dgtsv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var dl = new Float64Array( 0 );
	var d = new Float64Array( 0 );
	var du = new Float64Array( 0 );
	var B = new Float64Array( 0 );

	var info = dgtsv( 0, 1, dl, 1, 0, d, 1, 0, du, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info );
});

test( 'dgtsv: singular', function t() {
	var tc = findCase( 'singular' );
	var dl = new Float64Array( [ 0.0, 0.0 ] );
	var d = new Float64Array( [ 0.0, 2.0, 3.0 ] );
	var du = new Float64Array( [ 1.0, 1.0 ] );
	var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );

	var info = dgtsv( 3, 1, dl, 1, 0, d, 1, 0, du, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info );
});

test( 'dgtsv: pivoting', function t() {
	var tc = findCase( 'pivoting' );
	var N = 4;
	var dl = new Float64Array( [ 5.0, 7.0, 9.0 ] );
	var d = new Float64Array( [ 1.0, 3.0, 2.0, 1.0 ] );
	var du = new Float64Array( [ 2.0, 4.0, 6.0 ] );
	var B = new Float64Array( [ 5.0, 12.0, 15.0, 10.0 ] );

	var info = dgtsv( N, 1, dl, 1, 0, d, 1, 0, du, 1, 0, B, 1, N, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( toArray( B, 0, N ), tc.b, 1e-14, 'b' );
	assertArrayClose( toArray( d, 0, N ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( dl, 0, N - 1 ), tc.dl, 1e-14, 'dl' );
	assertArrayClose( toArray( du, 0, N - 1 ), tc.du, 1e-14, 'du' );
});

test( 'dgtsv: three_rhs', function t() {
	var tc = findCase( 'three_rhs' );
	var N = 3;
	var nrhs = 3;
	var LDB = N;
	var dl = new Float64Array( [ 1.0, 1.0 ] );
	var d = new Float64Array( [ 4.0, 4.0, 4.0 ] );
	var du = new Float64Array( [ 1.0, 1.0 ] );
	// Column-major: 3 columns of length 3
	var B = new Float64Array( [
		6.0, 9.0, 9.0,
		1.0, 2.0, 3.0,
		10.0, 5.0, 10.0
	] );

	var info = dgtsv( N, nrhs, dl, 1, 0, d, 1, 0, du, 1, 0, B, 1, LDB, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( getColumn( B, LDB, 0, N ), tc.b1, 1e-14, 'b1' );
	assertArrayClose( getColumn( B, LDB, 1, N ), tc.b2, 1e-14, 'b2' );
	assertArrayClose( getColumn( B, LDB, 2, N ), tc.b3, 1e-14, 'b3' );
	assertArrayClose( toArray( d, 0, N ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( dl, 0, N - 1 ), tc.dl, 1e-14, 'dl' );
	assertArrayClose( toArray( du, 0, N - 1 ), tc.du, 1e-14, 'du' );
});

test( 'dgtsv: pivoting_multi_rhs', function t() {
	var tc = findCase( 'pivoting_multi_rhs' );
	var N = 4;
	var nrhs = 2;
	var LDB = N;
	var dl = new Float64Array( [ 5.0, 7.0, 9.0 ] );
	var d = new Float64Array( [ 1.0, 3.0, 2.0, 1.0 ] );
	var du = new Float64Array( [ 2.0, 4.0, 6.0 ] );
	var B = new Float64Array( [
		5.0, 12.0, 15.0, 10.0,
		3.0, 7.0, 9.0, 10.0
	] );

	var info = dgtsv( N, nrhs, dl, 1, 0, d, 1, 0, du, 1, 0, B, 1, LDB, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( getColumn( B, LDB, 0, N ), tc.b1, 1e-14, 'b1' );
	assertArrayClose( getColumn( B, LDB, 1, N ), tc.b2, 1e-14, 'b2' );
	assertArrayClose( toArray( d, 0, N ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( dl, 0, N - 1 ), tc.dl, 1e-14, 'dl' );
	assertArrayClose( toArray( du, 0, N - 1 ), tc.du, 1e-14, 'du' );
});
