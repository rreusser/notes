'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlascl = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlascl.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// HELPERS //

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, msg ) {
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], msg + '[' + i + ']' );
	}
}

function c128( arr ) {
	return new Complex128Array( arr );
}

function extractCMatrix( Av, strideA1, strideA2, offsetA, M, N ) {
	var out = [];
	var ai;
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			ai = offsetA + 2 * ( i * strideA1 + j * strideA2 );
			out.push( Av[ ai ] );
			out.push( Av[ ai + 1 ] );
		}
	}
	return out;
}

// TESTS //

test( 'zlascl: main export is a function', function t() {
	assert.strictEqual( typeof zlascl, 'function' );
});

test( 'zlascl: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlascl.ndarray, 'function' );
});

test( 'zlascl: basic scaling (multiply by 2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_basic'; } );
	var A = c128( new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] ) );
	var info = base( 'G', 0, 0, 1.0, 2.0, 2, 2, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'zlascl_basic a' );
});

test( 'zlascl: scaling by 0.5 (cfrom=2, cto=1)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_half'; } );
	var A = c128( new Float64Array( [ 2, 4, 6, 8, 10, 12, 14, 16 ] ) );
	var info = base( 'G', 0, 0, 2.0, 1.0, 2, 2, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'zlascl_half a' );
});

test( 'zlascl: M=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_m_zero'; } );
	var A = c128( new Float64Array( [ 99, 88 ] ) );
	var info = base( 'G', 0, 0, 1.0, 2.0, 0, 2, A, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'zlascl_m_zero a' );
});

test( 'zlascl: upper triangular', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_upper'; } );
	var A = c128( new Float64Array( [
		1, 0,  0, 0,  0, 0,
		2, 0,  4, 0,  0, 0,
		3, 0,  5, 0,  6, 0
	] ) );
	var Av = reinterpret( A, 0 );
	var info = base( 'U', 0, 0, 1.0, 3.0, 3, 3, A, 1, 3, 0 );
	assert.strictEqual( info, tc.info );
	var result = extractCMatrix( Av, 1, 3, 0, 3, 3 );
	assertArrayClose( result, tc.a, 'zlascl_upper a' );
});

test( 'zlascl: lower triangular', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_lower'; } );
	var A = c128( new Float64Array( [
		1, 0,  2, 0,  3, 0,
		0, 0,  4, 0,  5, 0,
		0, 0,  0, 0,  6, 0
	] ) );
	var Av = reinterpret( A, 0 );
	var info = base( 'L', 0, 0, 1.0, 3.0, 3, 3, A, 1, 3, 0 );
	assert.strictEqual( info, tc.info );
	var result = extractCMatrix( Av, 1, 3, 0, 3, 3 );
	assertArrayClose( result, tc.a, 'zlascl_lower a' );
});

test( 'zlascl: identity (cfrom=cto)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_identity'; } );
	var A = c128( new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] ) );
	var info = base( 'G', 0, 0, 5.0, 5.0, 2, 2, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'zlascl_identity a' );
});

test( 'zlascl: upper Hessenberg', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_hessenberg'; } );
	var A = c128( new Float64Array( [
		1, 0,  2, 0,  0, 0,
		3, 0,  4, 0,  5, 0,
		6, 0,  7, 0,  8, 0
	] ) );
	var Av = reinterpret( A, 0 );
	var info = base( 'H', 0, 0, 1.0, 2.0, 3, 3, A, 1, 3, 0 );
	assert.strictEqual( info, tc.info );
	var result = extractCMatrix( Av, 1, 3, 0, 3, 3 );
	assertArrayClose( result, tc.a, 'zlascl_hessenberg a' );
});

test( 'zlascl: large cfrom/cto ratio (iterative scaling, mul=smlnum)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_large_ratio'; } );
	var A = c128( new Float64Array( [ 1, 1, 2, 3 ] ) );
	var info = base( 'G', 0, 0, 1e300, 1e-300, 2, 1, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'zlascl_large_ratio a' );
});

test( 'zlascl: large cto/cfrom ratio (iterative scaling, mul=bignum)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_large_ratio_inv'; } );
	var A = c128( new Float64Array( [ 1e-150, 1e-150, 2e-150, 3e-150 ] ) );
	var info = base( 'G', 0, 0, 1e-150, 1e150, 2, 1, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'zlascl_large_ratio_inv a' );
});

test( 'zlascl: lower band matrix (type B)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_lower_band'; } );
	var A = c128( new Float64Array( [
		1, 0,  4, 0,
		2, 0,  5, 0,
		3, 0,  0, 0
	] ) );
	var info = base( 'B', 1, 1, 1.0, 3.0, 3, 3, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'zlascl_lower_band a' );
});

test( 'zlascl: upper band matrix (type Q)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_upper_band'; } );
	var A = c128( new Float64Array( [
		0, 0,  3, 0,
		1, 0,  4, 0,
		2, 0,  5, 0
	] ) );
	var info = base( 'Q', 1, 1, 1.0, 3.0, 3, 3, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'zlascl_upper_band a' );
});

test( 'zlascl: full band matrix (type Z)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_band'; } );
	var A = c128( new Float64Array( [
		0, 0,  3, 0,  6, 0,  9, 0,
		1, 0,  4, 0,  7, 0,  10, 0,
		2, 0,  5, 0,  8, 0,  0, 0
	] ) );
	var Av = reinterpret( A, 0 );
	var info = base( 'Z', 1, 1, 1.0, 2.0, 3, 3, A, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	var result = extractCMatrix( Av, 1, 4, 0, 4, 3 );
	assertArrayClose( result, tc.a, 'zlascl_band a' );
});

test( 'zlascl: invalid type returns -1', function t() {
	var A = c128( new Float64Array( [ 1, 2, 3, 4 ] ) );
	var info = base( 'X', 0, 0, 1.0, 2.0, 1, 1, A, 1, 1, 0 );
	assert.strictEqual( info, -1 );
});
