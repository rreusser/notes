'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztrttf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztrttf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var denom;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch (actual=' + actual.length + ', expected=' + expected.length + ')' );
	for ( i = 0; i < expected.length; i += 1 ) {
		denom = Math.max( Math.abs( expected[ i ] ), 1.0 );
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / denom;
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] + ' (relErr=' + relErr + ')' );
	}
}


// TESTS //

test( 'ztrttf is a function', function t() {
	assert.equal( typeof ztrttf, 'function' );
});

test( 'ztrttf: N=0 quick return', function t() {
	var ARF;
	var A;

	A = new Complex128Array( 1 );
	ARF = new Complex128Array( 1 );
	assert.equal( ztrttf( 'no-transpose', 'lower', 0, A, 1, 1, 0, 1, ARF, 1, 0 ), 0 );
});

test( 'ztrttf: N=1, TRANSR=no-transpose (copies element)', function t() {
	var tc = findCase( 'n1_N' );
	var ARF;
	var arv;
	var A;

	A = new Complex128Array( [ 3.0, 4.0 ] );
	ARF = new Complex128Array( 1 );
	assert.equal( ztrttf( 'no-transpose', 'lower', 1, A, 1, 1, 0, 1, ARF, 1, 0 ), 0 );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=1, TRANSR=conjugate-transpose (conjugates element)', function t() {
	var tc = findCase( 'n1_C' );
	var ARF;
	var arv;
	var A;

	A = new Complex128Array( [ 3.0, 4.0 ] );
	ARF = new Complex128Array( 1 );
	assert.equal( ztrttf( 'conjugate-transpose', 'lower', 1, A, 1, 1, 0, 1, ARF, 1, 0 ), 0 );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=5, TRANSR=N, UPLO=L (odd, normal, lower)', function t() {
	var tc = findCase( 'n5_N_L' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'no-transpose', 'lower', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=5, TRANSR=N, UPLO=U (odd, normal, upper)', function t() {
	var tc = findCase( 'n5_N_U' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'no-transpose', 'upper', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=5, TRANSR=C, UPLO=L (odd, conj-transpose, lower)', function t() {
	var tc = findCase( 'n5_C_L' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'conjugate-transpose', 'lower', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=5, TRANSR=C, UPLO=U (odd, conj-transpose, upper)', function t() {
	var tc = findCase( 'n5_C_U' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'conjugate-transpose', 'upper', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=6, TRANSR=N, UPLO=L (even, normal, lower)', function t() {
	var tc = findCase( 'n6_N_L' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'no-transpose', 'lower', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=6, TRANSR=N, UPLO=U (even, normal, upper)', function t() {
	var tc = findCase( 'n6_N_U' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'no-transpose', 'upper', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=6, TRANSR=C, UPLO=L (even, conj-transpose, lower)', function t() {
	var tc = findCase( 'n6_C_L' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'conjugate-transpose', 'lower', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=6, TRANSR=C, UPLO=U (even, conj-transpose, upper)', function t() {
	var tc = findCase( 'n6_C_U' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'conjugate-transpose', 'upper', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=7, TRANSR=N, UPLO=L (odd, normal, lower)', function t() {
	var tc = findCase( 'n7_N_L' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'no-transpose', 'lower', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=7, TRANSR=N, UPLO=U (odd, normal, upper)', function t() {
	var tc = findCase( 'n7_N_U' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'no-transpose', 'upper', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=7, TRANSR=C, UPLO=L (odd, conj-transpose, lower)', function t() {
	var tc = findCase( 'n7_C_L' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'conjugate-transpose', 'lower', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=7, TRANSR=C, UPLO=U (odd, conj-transpose, upper)', function t() {
	var tc = findCase( 'n7_C_U' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'conjugate-transpose', 'upper', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=8, TRANSR=N, UPLO=L (even, normal, lower)', function t() {
	var tc = findCase( 'n8_N_L' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'no-transpose', 'lower', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=8, TRANSR=N, UPLO=U (even, normal, upper)', function t() {
	var tc = findCase( 'n8_N_U' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'no-transpose', 'upper', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=8, TRANSR=C, UPLO=L (even, conj-transpose, lower)', function t() {
	var tc = findCase( 'n8_C_L' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'conjugate-transpose', 'lower', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});

test( 'ztrttf: N=8, TRANSR=C, UPLO=U (even, conj-transpose, upper)', function t() {
	var tc = findCase( 'n8_C_U' );
	var info;
	var ARF;
	var arv;
	var A;
	var N;

	N = tc.n;
	A = new Complex128Array( tc.A );
	ARF = new Complex128Array( N * ( N + 1 ) / 2 );
	info = ztrttf( 'conjugate-transpose', 'upper', N, A, 1, N, 0, N, ARF, 1, 0 );
	assert.equal( info, tc.info );
	arv = reinterpret( ARF, 0 );
	assertArrayClose( Array.from( arv ), tc.ARF, 1e-14, 'ARF' );
});
