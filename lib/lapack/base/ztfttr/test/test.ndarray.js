

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztfttr = require( './../lib/ndarray.js' );

// FIXTURES //

var n0 = require( './fixtures/n0.json' );
var n1_n = require( './fixtures/n1_n.json' );
var n1_c = require( './fixtures/n1_c.json' );
var n5_n_l = require( './fixtures/n5_n_l.json' );
var n5_n_u = require( './fixtures/n5_n_u.json' );
var n5_c_l = require( './fixtures/n5_c_l.json' );
var n5_c_u = require( './fixtures/n5_c_u.json' );
var n6_n_l = require( './fixtures/n6_n_l.json' );
var n6_n_u = require( './fixtures/n6_n_u.json' );
var n6_c_l = require( './fixtures/n6_c_l.json' );
var n6_c_u = require( './fixtures/n6_c_u.json' );

// FUNCTIONS //

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
	var tc = n0;
	var ARF = new Complex128Array( 1 );
	var A = new Complex128Array( 1 );
	var info = ztfttr( 'no-transpose', 'lower', 0, ARF, 1, 0, A, 1, 1, 0, 1 );
	assert.equal( info, tc.info );
});

test( 'ztfttr: N=1 normal (no conjugation)', function t() {
	var tc = n1_n;
	var ARF = new Complex128Array( [ 42.0, 7.0 ] );
	var A = new Complex128Array( 1 );
	var Av;
	var info = ztfttr( 'no-transpose', 'lower', 1, ARF, 1, 0, A, 1, 1, 0, 1 );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=1 conjugate-transpose', function t() {
	var tc = n1_c;
	var ARF = new Complex128Array( [ 42.0, 7.0 ] );
	var A = new Complex128Array( 1 );
	var Av;
	var info = ztfttr( 'conjugate-transpose', 'lower', 1, ARF, 1, 0, A, 1, 1, 0, 1 );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});

test( 'ztfttr: N=5, TRANSR=no-transpose, UPLO=lower (odd, normal, lower)', function t() {
	var tc = n5_n_l;
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
	var tc = n5_n_u;
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
	var tc = n5_c_l;
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
	var tc = n5_c_u;
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
	var tc = n6_n_l;
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
	var tc = n6_n_u;
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
	var tc = n6_c_l;
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
	var tc = n6_c_u;
	var N = tc.n;
	var ARF = new Complex128Array( tc.ARF );
	var A = new Complex128Array( N * N );
	var Av;
	var info = ztfttr( 'conjugate-transpose', 'upper', N, ARF, 1, 0, A, 1, N, 0, N );
	assert.equal( info, tc.info );
	Av = reinterpret( A, 0 );
	assertArrayClose( Array.from( Av ), tc.A, 1e-14, 'A' );
});
