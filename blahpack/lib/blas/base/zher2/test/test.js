

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zher2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zher2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zher2: upper_basic (UPLO=U, N=2, alpha=(1,0))', function t() {
	var tc = findCase( 'upper_basic' );
	// A = [2 (1+i); . 3] (upper, column-major)
	var A = new Complex128Array( [
		2, 0, 0, 0,
		1, 1, 3, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	var y = new Complex128Array( [ 1, 1, 2, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = zher2( 'U', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: lower_basic (UPLO=L, N=2, alpha=(1,0))', function t() {
	var tc = findCase( 'lower_basic' );
	// A = [2 .; (1-i) 3] (lower, column-major)
	var A = new Complex128Array( [
		2, 0, 1, -1,
		0, 0, 3, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	var y = new Complex128Array( [ 1, 1, 2, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = zher2( 'L', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: n_zero (N=0 quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( [ 99, 88 ] );
	var x = new Complex128Array( 0 );
	var y = new Complex128Array( 0 );
	var alpha = new Complex128( 1, 0 );
	var result = zher2( 'U', 0, alpha, x, 1, 0, y, 1, 0, A, 1, 1, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: alpha_zero (alpha=0 quick return)', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Complex128Array( [ 99, 88, 0, 0, 0, 0, 0, 0 ] );
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	var y = new Complex128Array( [ 1, 1, 2, 0 ] );
	var alpha = new Complex128( 0, 0 );
	var result = zher2( 'U', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	// Fixture only prints first element; verify A is unchanged
	var Av = Array.from( reinterpret( A, 0 ) );
	assertArrayClose( Av.slice( 0, 2 ), tc.A, 1e-14, 'A' );
	// Rest should remain zero
	assert.deepStrictEqual( Av.slice( 2 ), [ 0, 0, 0, 0, 0, 0 ] );
});

test( 'zher2: n_one (N=1)', function t() {
	var tc = findCase( 'n_one' );
	// A(1,1)=5, x=(2,3), y=(1,-1), alpha=(1,0)
	var A = new Complex128Array( [ 5, 0 ] );
	var x = new Complex128Array( [ 2, 3 ] );
	var y = new Complex128Array( [ 1, -1 ] );
	var alpha = new Complex128( 1, 0 );
	var result = zher2( 'U', 1, alpha, x, 1, 0, y, 1, 0, A, 1, 1, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: upper_stride (UPLO=U, incx=2, incy=2)', function t() {
	var tc = findCase( 'upper_stride' );
	// 3x3 Hermitian upper
	var A = new Complex128Array( [
		2, 0, 0, 0, 0, 0,
		1, 1, 3, 0, 0, 0,
		2, -1, 1, 2, 4, 0
	] );
	// x with stride=2: complex indices 0, 2, 4 → values (1,0), (0,1), (1,1)
	var x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	// y with stride=2: complex indices 0, 2, 4 → values (1,1), (2,0), (0,-1)
	var y = new Complex128Array( [ 1, 1, 0, 0, 2, 0, 0, 0, 0, -1 ] );
	var alpha = new Complex128( 1, 0 );
	var result = zher2( 'U', 3, alpha, x, 2, 0, y, 2, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: lower_stride (UPLO=L, incx=2, incy=2)', function t() {
	var tc = findCase( 'lower_stride' );
	// 3x3 Hermitian lower
	var A = new Complex128Array( [
		2, 0, 1, -1, 2, 1,
		0, 0, 3, 0, 1, -2,
		0, 0, 0, 0, 4, 0
	] );
	var x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	var y = new Complex128Array( [ 1, 1, 0, 0, 2, 0, 0, 0, 0, -1 ] );
	var alpha = new Complex128( 1, 0 );
	var result = zher2( 'L', 3, alpha, x, 2, 0, y, 2, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: complex_alpha (alpha=(2,1))', function t() {
	var tc = findCase( 'complex_alpha' );
	// 2x2 upper: A = [1 (0,1); . 2]
	var A = new Complex128Array( [
		1, 0, 0, 0,
		0, 1, 2, 0
	] );
	var x = new Complex128Array( [ 1, 0, 1, 0 ] );
	var y = new Complex128Array( [ 1, 0, 0, 1 ] );
	var alpha = new Complex128( 2, 1 );
	var result = zher2( 'U', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: upper_3x3 (UPLO=U, N=3, larger matrix)', function t() {
	var tc = findCase( 'upper_3x3' );
	// 3x3 upper: A = [1 (2+i) (0,-1); . 3 (1+i); . . 5]
	var A = new Complex128Array( [
		1, 0, 0, 0, 0, 0,
		2, 1, 3, 0, 0, 0,
		0, -1, 1, 1, 5, 0
	] );
	var x = new Complex128Array( [ 1, 1, 2, 0, 0, 3 ] );
	var y = new Complex128Array( [ 0, 1, 1, -1, 2, 2 ] );
	var alpha = new Complex128( 1, 0 );
	var result = zher2( 'U', 3, alpha, x, 1, 0, y, 1, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: lower_zeros (UPLO=L, x and y with zero elements — tests skip branch)', function t() {
	// 2x2 lower: A(0,0)=(1,0.5), A(1,0)=(2,1), A(1,1)=(3,0.5)
	// x = [0, 1], y = [0, 2] — first element zero triggers skip branch
	var A = new Complex128Array( [
		1, 0.5, 2, 1,
		0, 0, 3, 0.5
	] );
	var x = new Complex128Array( [ 0, 0, 1, 0 ] );
	var y = new Complex128Array( [ 0, 0, 2, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = zher2( 'L', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	var Av = Array.from( reinterpret( A, 0 ) );
	// Column 0: x[0]=y[0]=0 → skip → diagonal imag forced to 0
	assert.strictEqual( Av[ 0 ], 1.0, 'A(0,0) real unchanged' );
	assert.strictEqual( Av[ 1 ], 0.0, 'A(0,0) imag forced to 0' );
	// Column 1: x[1]=(1,0), y[1]=(2,0), update occurs
	// A(1,1) = real(3) + real(x[1]*temp1 + y[1]*temp2) = 3 + 2 + 2 = 7
	assert.strictEqual( Av[ 6 ], 7.0, 'A(1,1) updated' );
	assert.strictEqual( Av[ 7 ], 0.0, 'A(1,1) imag is 0' );
});

test( 'zher2: upper_zeros (x and y with zero elements — tests skip branch)', function t() {
	var tc = findCase( 'upper_zeros' );
	// 2x2 upper: A = [1 (2+i); . 3]
	var A = new Complex128Array( [
		1, 0, 0, 0,
		2, 1, 3, 0
	] );
	var x = new Complex128Array( [ 0, 0, 1, 0 ] );
	var y = new Complex128Array( [ 0, 0, 2, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = zher2( 'U', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});
