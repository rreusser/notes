'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgtsvx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgtsvx.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function toF64( arr ) {
	return new Float64Array( arr );
}


// TESTS //

test( 'dgtsvx: fact_n_trans_n', function t() {
	var tc = findCase( 'fact_n_trans_n' );
	var N = 4;
	var dl = toF64( [ 3.0, 1.0, 2.0 ] );
	var d = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = toF64( [ -1.0, -2.0, -3.0 ] );
	var dlf = new Float64Array( 3 );
	var df = new Float64Array( 4 );
	var duf = new Float64Array( 3 );
	var du2 = new Float64Array( 2 );
	var ipiv = new Int32Array( 4 );
	var b = toF64( [ 0.0, 5.0, 5.0, 30.0 ] );
	var x = new Float64Array( 4 );
	var rcond = new Float64Array( 1 );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 3 * N );
	var iwork = new Int32Array( N );

	var info = dgtsvx( 'not-factored', 'no-transpose', N, 1,
		dl, 1, 0, d, 1, 0, du, 1, 0,
		dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0,
		ipiv, 1, 0,
		b, 1, N, 0,
		x, 1, N, 0,
		rcond,
		ferr, 1, 0, berr, 1, 0,
		work, 1, 0, iwork, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
});

test( 'dgtsvx: fact_f_trans_n (already factored)', function t() {
	var tc = findCase( 'fact_f_trans_n' );
	var N = 4;
	var dl = toF64( [ 3.0, 1.0, 2.0 ] );
	var d = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = toF64( [ -1.0, -2.0, -3.0 ] );

	// Factor first
	var dlf = toF64( [ 3.0, 1.0, 2.0 ] );
	var df = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	var duf = toF64( [ -1.0, -2.0, -3.0 ] );
	var du2 = new Float64Array( 2 );
	var ipiv = new Int32Array( 4 );
	var dgttrf = require( './../../dgttrf/lib/base.js' );
	dgttrf( N, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0 );

	var b = toF64( [ 0.0, 5.0, 5.0, 30.0 ] );
	var x = new Float64Array( 4 );
	var rcond = new Float64Array( 1 );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 3 * N );
	var iwork = new Int32Array( N );

	var info = dgtsvx( 'factored', 'no-transpose', N, 1,
		dl, 1, 0, d, 1, 0, du, 1, 0,
		dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0,
		ipiv, 1, 0,
		b, 1, N, 0,
		x, 1, N, 0,
		rcond,
		ferr, 1, 0, berr, 1, 0,
		work, 1, 0, iwork, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
});

test( 'dgtsvx: fact_n_trans_t', function t() {
	var tc = findCase( 'fact_n_trans_t' );
	var N = 4;
	var dl = toF64( [ 3.0, 1.0, 2.0 ] );
	var d = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = toF64( [ -1.0, -2.0, -3.0 ] );
	var dlf = new Float64Array( 3 );
	var df = new Float64Array( 4 );
	var duf = new Float64Array( 3 );
	var du2 = new Float64Array( 2 );
	var ipiv = new Int32Array( 4 );
	var b = toF64( [ 8.0, 10.0, 19.0, 15.0 ] );
	var x = new Float64Array( 4 );
	var rcond = new Float64Array( 1 );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 3 * N );
	var iwork = new Int32Array( N );

	var info = dgtsvx( 'not-factored', 'transpose', N, 1,
		dl, 1, 0, d, 1, 0, du, 1, 0,
		dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0,
		ipiv, 1, 0,
		b, 1, N, 0,
		x, 1, N, 0,
		rcond,
		ferr, 1, 0, berr, 1, 0,
		work, 1, 0, iwork, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
});

test( 'dgtsvx: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var N = 4;
	var nrhs = 2;
	var dl = toF64( [ 3.0, 1.0, 2.0 ] );
	var d = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = toF64( [ -1.0, -2.0, -3.0 ] );
	var dlf = new Float64Array( 3 );
	var df = new Float64Array( 4 );
	var duf = new Float64Array( 3 );
	var du2 = new Float64Array( 2 );
	var ipiv = new Int32Array( 4 );
	var b = toF64( [ 0.0, 5.0, 5.0, 30.0, 4.0, 4.0, -4.0, 20.0 ] );
	var x = new Float64Array( N * nrhs );
	var rcond = new Float64Array( 1 );
	var ferr = new Float64Array( 2 );
	var berr = new Float64Array( 2 );
	var work = new Float64Array( 3 * N );
	var iwork = new Int32Array( N );

	var info = dgtsvx( 'not-factored', 'no-transpose', N, nrhs,
		dl, 1, 0, d, 1, 0, du, 1, 0,
		dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0,
		ipiv, 1, 0,
		b, 1, N, 0,
		x, 1, N, 0,
		rcond,
		ferr, 1, 0, berr, 1, 0,
		work, 1, 0, iwork, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( Array.from( x.subarray( 0, 4 ) ), tc.x1, 1e-14, 'x1' );
	assertArrayClose( Array.from( x.subarray( 4, 8 ) ), tc.x2, 1e-14, 'x2' );
});

test( 'dgtsvx: n_one', function t() {
	var tc = findCase( 'n_one' );
	var d = toF64( [ 5.0 ] );
	var dl = new Float64Array( 0 );
	var du = new Float64Array( 0 );
	var dlf = new Float64Array( 0 );
	var df = new Float64Array( 1 );
	var duf = new Float64Array( 0 );
	var du2 = new Float64Array( 0 );
	var ipiv = new Int32Array( 1 );
	var b = toF64( [ 10.0 ] );
	var x = new Float64Array( 1 );
	var rcond = new Float64Array( 1 );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 3 );
	var iwork = new Int32Array( 1 );

	var info = dgtsvx( 'not-factored', 'no-transpose', 1, 1,
		dl, 1, 0, d, 1, 0, du, 1, 0,
		dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0,
		ipiv, 1, 0,
		b, 1, 1, 0,
		x, 1, 1, 0,
		rcond,
		ferr, 1, 0, berr, 1, 0,
		work, 1, 0, iwork, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
});

test( 'dgtsvx: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var dl = new Float64Array( 0 );
	var d = new Float64Array( 0 );
	var du = new Float64Array( 0 );
	var dlf = new Float64Array( 0 );
	var df = new Float64Array( 0 );
	var duf = new Float64Array( 0 );
	var du2 = new Float64Array( 0 );
	var ipiv = new Int32Array( 0 );
	var b = new Float64Array( 0 );
	var x = new Float64Array( 0 );
	var rcond = new Float64Array( 1 );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 1 );
	var iwork = new Int32Array( 1 );

	var info = dgtsvx( 'not-factored', 'no-transpose', 0, 1,
		dl, 1, 0, d, 1, 0, du, 1, 0,
		dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0,
		ipiv, 1, 0,
		b, 1, 0, 0,
		x, 1, 0, 0,
		rcond,
		ferr, 1, 0, berr, 1, 0,
		work, 1, 0, iwork, 1, 0 );

	assert.equal( info, tc.info );
});

test( 'dgtsvx: singular', function t() {
	var tc = findCase( 'singular' );
	var N = 3;
	var dl = toF64( [ 0.0, 0.0 ] );
	var d = toF64( [ 0.0, 2.0, 3.0 ] );
	var du = toF64( [ 1.0, 1.0 ] );
	var dlf = new Float64Array( 2 );
	var df = new Float64Array( 3 );
	var duf = new Float64Array( 2 );
	var du2 = new Float64Array( 1 );
	var ipiv = new Int32Array( 3 );
	var b = toF64( [ 1.0, 2.0, 3.0 ] );
	var x = new Float64Array( 3 );
	var rcond = new Float64Array( 1 );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 3 * N );
	var iwork = new Int32Array( N );

	var info = dgtsvx( 'not-factored', 'no-transpose', N, 1,
		dl, 1, 0, d, 1, 0, du, 1, 0,
		dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0,
		ipiv, 1, 0,
		b, 1, N, 0,
		x, 1, N, 0,
		rcond,
		ferr, 1, 0, berr, 1, 0,
		work, 1, 0, iwork, 1, 0 );

	assert.equal( info, tc.info );
	assert.equal( rcond[ 0 ], tc.rcond );
});

test( 'dgtsvx: pivot_5x5', function t() {
	var tc = findCase( 'pivot_5x5' );
	var N = 5;
	var dl = toF64( [ 5.0, 7.0, 9.0, 2.0 ] );
	var d = toF64( [ 1.0, 3.0, 2.0, 1.0, 8.0 ] );
	var du = toF64( [ 2.0, 4.0, 6.0, 3.0 ] );
	var dlf = new Float64Array( 4 );
	var df = new Float64Array( 5 );
	var duf = new Float64Array( 4 );
	var du2 = new Float64Array( 3 );
	var ipiv = new Int32Array( 5 );
	var b = toF64( [ 3.0, 12.0, 15.0, 13.0, 10.0 ] );
	var x = new Float64Array( 5 );
	var rcond = new Float64Array( 1 );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 3 * N );
	var iwork = new Int32Array( N );

	var info = dgtsvx( 'not-factored', 'no-transpose', N, 1,
		dl, 1, 0, d, 1, 0, du, 1, 0,
		dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0,
		ipiv, 1, 0,
		b, 1, N, 0,
		x, 1, N, 0,
		rcond,
		ferr, 1, 0, berr, 1, 0,
		work, 1, 0, iwork, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
});

test( 'dgtsvx: fact_n_trans_c (conjugate-transpose)', function t() {
	var tc = findCase( 'fact_n_trans_c' );
	var N = 4;
	var dl = toF64( [ 3.0, 1.0, 2.0 ] );
	var d = toF64( [ 2.0, 4.0, 5.0, 6.0 ] );
	var du = toF64( [ -1.0, -2.0, -3.0 ] );
	var dlf = new Float64Array( 3 );
	var df = new Float64Array( 4 );
	var duf = new Float64Array( 3 );
	var du2 = new Float64Array( 2 );
	var ipiv = new Int32Array( 4 );
	var b = toF64( [ 8.0, 10.0, 19.0, 15.0 ] );
	var x = new Float64Array( 4 );
	var rcond = new Float64Array( 1 );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 3 * N );
	var iwork = new Int32Array( N );

	var info = dgtsvx( 'not-factored', 'conjugate-transpose', N, 1,
		dl, 1, 0, d, 1, 0, du, 1, 0,
		dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0,
		ipiv, 1, 0,
		b, 1, N, 0,
		x, 1, N, 0,
		rcond,
		ferr, 1, 0, berr, 1, 0,
		work, 1, 0, iwork, 1, 0 );

	assert.equal( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
});
