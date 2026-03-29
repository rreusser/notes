

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztfttr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztfttr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}


// TESTS //

test( 'ztfttr is a function', function t() {
	assert.equal( typeof ztfttr, 'function' );
});

test( 'ztfttr: N=0 quick return', function t() {
	var tc = findCase( 'n0' );
	var ARF = new Complex128Array( 1 );
	var A = new Complex128Array( 1 );
	var info = ztfttr( 'no-transpose', 'lower', 0, ARF, 1, 0, A, 1, 1, 0, 1 );
	assert.equal( info, tc.info );
});

test( 'ztfttr: N=1 normal (no conjugation)', function t() {
	var tc = findCase( 'n1_N' );
	var ARF = new Complex128Array( [ 42.0, 7.0 ] );
	var A = new Complex128Array( 1 );
	var Av;
	var info = ztfttr( 'no-transpose', 'lower', 1, ARF, 1, 0, A, 1, 1, 0, 1 );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=1 conjugate-transpose', function t() {
	var tc = findCase( 'n1_C' );
	var ARF = new Complex128Array( [ 42.0, 7.0 ] );
	var A = new Complex128Array( 1 );
	var Av;
	var info = ztfttr( 'conjugate-transpose', 'lower', 1, ARF, 1, 0, A, 1, 1, 0, 1 );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=5, TRANSR=no-transpose, UPLO=lower (odd, normal, lower)', function t() {
	var tc = findCase( 'n5_N_L' );
	var N = tc.n;
	var NT = N * ( N + 1 ) / 2;
	var ARF = new Complex128Array( tc.ARF );
	var A = new Complex128Array( N * N );
	var Av;
	var info = ztfttr( 'no-transpose', 'lower', N, ARF, 1, 0, A, 1, N, 0, N );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=5, TRANSR=no-transpose, UPLO=upper (odd, normal, upper)', function t() {
	var tc = findCase( 'n5_N_U' );
	var N = tc.n;
	var ARF = new Complex128Array( tc.ARF );
	var A = new Complex128Array( N * N );
	var Av;
	var info = ztfttr( 'no-transpose', 'upper', N, ARF, 1, 0, A, 1, N, 0, N );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=5, TRANSR=conjugate-transpose, UPLO=lower (odd, conj-trans, lower)', function t() {
	var tc = findCase( 'n5_C_L' );
	var N = tc.n;
	var ARF = new Complex128Array( tc.ARF );
	var A = new Complex128Array( N * N );
	var Av;
	var info = ztfttr( 'conjugate-transpose', 'lower', N, ARF, 1, 0, A, 1, N, 0, N );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=5, TRANSR=conjugate-transpose, UPLO=upper (odd, conj-trans, upper)', function t() {
	var tc = findCase( 'n5_C_U' );
	var N = tc.n;
	var ARF = new Complex128Array( tc.ARF );
	var A = new Complex128Array( N * N );
	var Av;
	var info = ztfttr( 'conjugate-transpose', 'upper', N, ARF, 1, 0, A, 1, N, 0, N );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=6, TRANSR=no-transpose, UPLO=lower (even, normal, lower)', function t() {
	var tc = findCase( 'n6_N_L' );
	var N = tc.n;
	var ARF = new Complex128Array( tc.ARF );
	var A = new Complex128Array( N * N );
	var Av;
	var info = ztfttr( 'no-transpose', 'lower', N, ARF, 1, 0, A, 1, N, 0, N );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=6, TRANSR=no-transpose, UPLO=upper (even, normal, upper)', function t() {
	var tc = findCase( 'n6_N_U' );
	var N = tc.n;
	var ARF = new Complex128Array( tc.ARF );
	var A = new Complex128Array( N * N );
	var Av;
	var info = ztfttr( 'no-transpose', 'upper', N, ARF, 1, 0, A, 1, N, 0, N );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=6, TRANSR=conjugate-transpose, UPLO=lower (even, conj-trans, lower)', function t() {
	var tc = findCase( 'n6_C_L' );
	var N = tc.n;
	var ARF = new Complex128Array( tc.ARF );
	var A = new Complex128Array( N * N );
	var Av;
	var info = ztfttr( 'conjugate-transpose', 'lower', N, ARF, 1, 0, A, 1, N, 0, N );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=6, TRANSR=conjugate-transpose, UPLO=upper (even, conj-trans, upper)', function t() {
	var tc = findCase( 'n6_C_U' );
	var N = tc.n;
	var ARF = new Complex128Array( tc.ARF );
	var A = new Complex128Array( N * N );
	var Av;
	var info = ztfttr( 'conjugate-transpose', 'upper', N, ARF, 1, 0, A, 1, N, 0, N );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});
