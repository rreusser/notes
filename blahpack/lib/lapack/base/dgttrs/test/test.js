'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgttrf = require( './../../dgttrf/lib/base.js' );
var dgttrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgttrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Factorize a tridiagonal matrix using dgttrf and convert IPIV to 0-based.
* Returns { dl, d, du, du2, ipiv, info }.
*/
function factorize( dlArr, dArr, duArr, n ) {
	var dl = new Float64Array( dlArr );
	var d = new Float64Array( dArr );
	var du = new Float64Array( duArr );
	var du2 = new Float64Array( Math.max( n - 2, 0 ) );
	var ipiv = new Int32Array( n );
	var info;

	info = dgttrf( n, dl, 1, 0, d, 1, 0, du, 1, 0, du2, 1, 0, ipiv, 1, 0 );
	return { dl: dl, d: d, du: du, du2: du2, ipiv: ipiv, info: info };
}


// TESTS //

test( 'dgttrs: no-transpose, single RHS, 5x5 symmetric tridiagonal', function t() {
	var tc = findCase( 'notrans_single_rhs' );
	var n = 5;
	var nrhs = 1;
	var f = factorize( [ -1, -1, -1, -1 ], [ 2, 2, 2, 2, 2 ], [ -1, -1, -1, -1 ], n );
	// B is column-major: n rows x nrhs cols, strideB1=1, strideB2=n
	var B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	var info;

	assert.equal( f.info, 0 );
	info = dgttrs( 'N', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgttrs: transpose, single RHS, 5x5 symmetric tridiagonal', function t() {
	var tc = findCase( 'trans_single_rhs' );
	var n = 5;
	var nrhs = 1;
	var f = factorize( [ -1, -1, -1, -1 ], [ 2, 2, 2, 2, 2 ], [ -1, -1, -1, -1 ], n );
	var B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	var info;

	assert.equal( f.info, 0 );
	info = dgttrs( 'T', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgttrs: no-transpose, multiple RHS (3 columns)', function t() {
	var tc = findCase( 'notrans_multi_rhs' );
	var n = 5;
	var nrhs = 3;
	var f = factorize( [ -1, -1, -1, -1 ], [ 2, 2, 2, 2, 2 ], [ -1, -1, -1, -1 ], n );
	// Column-major: B stored as n rows x nrhs cols, strideB1=1, strideB2=n
	var B = new Float64Array( [
		1, 0, 0, 0, 1,  // column 1
		1, 1, 1, 1, 1,  // column 2
		0, 0, 1, 0, 0   // column 3
	] );
	var info;

	assert.equal( f.info, 0 );
	info = dgttrs( 'N', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgttrs: transpose, multiple RHS (3 columns)', function t() {
	var tc = findCase( 'trans_multi_rhs' );
	var n = 5;
	var nrhs = 3;
	var f = factorize( [ -1, -1, -1, -1 ], [ 2, 2, 2, 2, 2 ], [ -1, -1, -1, -1 ], n );
	var B = new Float64Array( [
		1, 0, 0, 0, 1,
		1, 1, 1, 1, 1,
		0, 0, 1, 0, 0
	] );
	var info;

	assert.equal( f.info, 0 );
	info = dgttrs( 'T', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgttrs: N=1', function t() {
	var tc = findCase( 'n_one' );
	var n = 1;
	var nrhs = 1;
	var f = factorize( [], [ 5 ], [], n );
	var B = new Float64Array( [ 10 ] );
	var info;

	assert.equal( f.info, 0 );
	info = dgttrs( 'N', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgttrs: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var info;

	info = dgttrs( 'N', 0, 1, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Int32Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'dgttrs: pivoting, no-transpose', function t() {
	var tc = findCase( 'pivot_notrans' );
	var n = 5;
	var nrhs = 1;
	var f = factorize( [ 10, 10, 10, 10 ], [ 1, 1, 1, 1, 1 ], [ 2, 2, 2, 2 ], n );
	var B = new Float64Array( [ 3, 13, 13, 13, 11 ] );
	var info;

	assert.equal( f.info, 0 );
	info = dgttrs( 'N', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-13, 'B' );
});

test( 'dgttrs: pivoting, transpose', function t() {
	var tc = findCase( 'pivot_trans' );
	var n = 5;
	var nrhs = 1;
	var f = factorize( [ 10, 10, 10, 10 ], [ 1, 1, 1, 1, 1 ], [ 2, 2, 2, 2 ], n );
	var B = new Float64Array( [ 3, 13, 13, 13, 11 ] );
	var info;

	assert.equal( f.info, 0 );
	info = dgttrs( 'T', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-13, 'B' );
});

test( 'dgttrs: N=2, no-transpose', function t() {
	var tc = findCase( 'n_two_notrans' );
	var n = 2;
	var nrhs = 1;
	var f = factorize( [ 3 ], [ 4, 7 ], [ 1 ], n );
	var B = new Float64Array( [ 5, 10 ] );
	var info;

	assert.equal( f.info, 0 );
	info = dgttrs( 'N', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgttrs: N=2, transpose', function t() {
	var tc = findCase( 'n_two_trans' );
	var n = 2;
	var nrhs = 1;
	var f = factorize( [ 3 ], [ 4, 7 ], [ 1 ], n );
	var B = new Float64Array( [ 5, 10 ] );
	var info;

	assert.equal( f.info, 0 );
	info = dgttrs( 'T', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgttrs: supports conjugate transpose (C) same as transpose', function t() {
	var tc = findCase( 'trans_single_rhs' );
	var n = 5;
	var nrhs = 1;
	var f = factorize( [ -1, -1, -1, -1 ], [ 2, 2, 2, 2, 2 ], [ -1, -1, -1, -1 ], n );
	var B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	var info;

	info = dgttrs( 'C', n, nrhs, f.dl, 1, 0, f.d, 1, 0, f.du, 1, 0, f.du2, 1, 0, f.ipiv, 1, 0, B, 1, n, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( B, new Float64Array( tc.B ), 1e-14, 'B' );
});

test( 'dgttrs: NRHS=0 quick return', function t() {
	var info;

	info = dgttrs( 'N', 5, 0, new Float64Array( 4 ), 1, 0, new Float64Array( 5 ), 1, 0, new Float64Array( 4 ), 1, 0, new Float64Array( 3 ), 1, 0, new Int32Array( 5 ), 1, 0, new Float64Array( 0 ), 1, 5, 0 );

	assert.equal( info, 0, 'info' );
});
