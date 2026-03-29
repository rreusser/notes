/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhpr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhpr.jsonl' ), 'utf8' ).trim().split( '\n' );
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
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zhpr is a function', function t() {
	assert.strictEqual( typeof zhpr, 'function' );
});

test( 'zhpr: upper_basic (uplo=U, N=3, alpha=2, unit stride)', function t() {
	var result = zhpr( 'upper', 3, 2.0, x, 1, 0, AP, 1, 0 );
	var APv = reinterpret( AP, 0 );
	var tc = findCase( 'upper_basic' );
	var AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		1.0,
		4.0,
		0.0,
		3.0,
		-2.0,
		2.0,
		1.0,
		5.0,
		0.0
	]);
	var x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	assertArrayClose( Array.from( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: lower_basic (uplo=L, N=3, alpha=2, unit stride)', function t() {
	var result = zhpr( 'lower', 3, 2.0, x, 1, 0, AP, 1, 0 );
	var APv = reinterpret( AP, 0 );
	var tc = findCase( 'lower_basic' );
	var AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		-1.0,
		3.0,
		2.0,
		4.0,
		0.0,
		2.0,
		-1.0,
		5.0,
		0.0
	]);
	var x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	assertArrayClose( Array.from( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: alpha_zero (alpha=0, no-op)', function t() {
	var result = zhpr( 'upper', 3, 0.0, x, 1, 0, AP, 1, 0 );
	var APv = reinterpret( AP, 0 );
	var tc = findCase( 'alpha_zero' );
	var AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		1.0,
		4.0,
		0.0,
		3.0,
		-2.0,
		2.0,
		1.0,
		5.0,
		0.0
	]);
	var x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	assertArrayClose( Array.from( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: n_zero (N=0 quick return)', function t() {
	var result = zhpr( 'upper', 0, 1.0, x, 1, 0, AP, 1, 0 );
	var APv = reinterpret( AP, 0 );
	var tc = findCase( 'n_zero' );
	var AP = new Complex128Array( [ 99.0, 0.0 ] );
	var x = new Complex128Array( [ 1.0, 0.5 ] );
	assertArrayClose( Array.from( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: scalar (N=1, alpha=1.5)', function t() {
	var result = zhpr( 'upper', 1, 1.5, x, 1, 0, AP, 1, 0 );
	var APv = reinterpret( AP, 0 );
	var tc = findCase( 'scalar' );
	var AP = new Complex128Array( [ 3.0, 0.0 ] );
	var x = new Complex128Array( [ 2.0, 1.0 ] );
	assertArrayClose( Array.from( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: upper_stride_2 (uplo=U, N=3, strideX=2)', function t() {
	var result = zhpr( 'upper', 3, 2.0, x, 2, 0, AP, 1, 0 );
	var APv = reinterpret( AP, 0 );
	var tc = findCase( 'upper_stride_2' );
	var AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		1.0,
		4.0,
		0.0,
		3.0,
		-2.0,
		2.0,
		1.0,
		5.0,
		0.0
	]);
	var x = new Complex128Array([
		1.0,
		0.5,
		0.0,
		0.0,
		2.0,
		-1.0,
		0.0,
		0.0,
		3.0,
		1.0,
		0.0,
		0.0
	]);
	assertArrayClose( Array.from( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: zero_element (x[1]=0, exercises skip branch)', function t() {
	var result = zhpr( 'upper', 3, 2.0, x, 1, 0, AP, 1, 0 );
	var APv = reinterpret( AP, 0 );
	var tc = findCase( 'zero_element' );
	var AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		1.0,
		4.0,
		0.0,
		3.0,
		-2.0,
		2.0,
		1.0,
		5.0,
		0.0
	]);
	var x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 3.0, 1.0 ] );
	assertArrayClose( Array.from( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: lower_stride_2 (uplo=L, N=3, strideX=2)', function t() {
	var result = zhpr( 'lower', 3, 2.0, x, 2, 0, AP, 1, 0 );
	var APv = reinterpret( AP, 0 );
	var tc = findCase( 'lower_stride_2' );
	var AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		-1.0,
		3.0,
		2.0,
		4.0,
		0.0,
		2.0,
		-1.0,
		5.0,
		0.0
	]);
	var x = new Complex128Array([
		1.0,
		0.5,
		0.0,
		0.0,
		2.0,
		-1.0,
		0.0,
		0.0,
		3.0,
		1.0,
		0.0,
		0.0
	]);
	assertArrayClose( Array.from( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: returns AP', function t() {
	var result = zhpr( 'upper', 1, 1.0, x, 1, 0, AP, 1, 0 );
	var AP = new Complex128Array( [ 1.0, 0.0 ] );
	var x = new Complex128Array( [ 1.0, 0.0 ] );
	assert.strictEqual( result, AP );
});

test( 'zhpr: all zero x leaves AP unchanged except diagonal imaginary forced to zero', function t() {
	var APv = reinterpret( AP, 0 );
	var AP = new Complex128Array([
		1.0,
		0.5,
		2.0,
		1.0,
		3.0,
		0.7,
		4.0,
		0.3,
		5.0,
		0.2,
		6.0,
		0.9
	]);
	var x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );

	// Off-diagonal should be unchanged, but diagonal imaginary parts zeroed

	// Upper packed N=3: positions 0(diag), 1, 2(diag), 3, 4, 5(diag)

	// In float64: diag at indices 0-1, 4-5, 10-11
	assert.strictEqual( APv[ 0 ], 1.0 );
	assert.strictEqual( APv[ 1 ], 0.0 ); // forced real
	assert.strictEqual( APv[ 2 ], 2.0 );
	assert.strictEqual( APv[ 3 ], 1.0 );
	assert.strictEqual( APv[ 4 ], 3.0 );
	assert.strictEqual( APv[ 5 ], 0.0 ); // forced real
	assert.strictEqual( APv[ 6 ], 4.0 );
	assert.strictEqual( APv[ 7 ], 0.3 );
	assert.strictEqual( APv[ 8 ], 5.0 );
	assert.strictEqual( APv[ 9 ], 0.2 );
	assert.strictEqual( APv[ 10 ], 6.0 );
	assert.strictEqual( APv[ 11 ], 0.0 ); // forced real
});

test( 'zhpr: lower with all zero x forces diagonal imaginary to zero', function t() {
	var APv = reinterpret( AP, 0 );
	var AP = new Complex128Array([
		1.0,
		0.5,
		2.0,
		1.0,
		3.0,
		0.7,
		4.0,
		0.3,
		5.0,
		0.2,
		6.0,
		0.9
	]);
	var x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );

	// Lower packed N=3: positions 0(diag), 1, 2, 3(diag), 4, 5(diag)

	// In float64: diag at indices 0-1, 6-7, 10-11
	assert.strictEqual( APv[ 0 ], 1.0 );
	assert.strictEqual( APv[ 1 ], 0.0 ); // forced real
	assert.strictEqual( APv[ 2 ], 2.0 );
	assert.strictEqual( APv[ 3 ], 1.0 );
	assert.strictEqual( APv[ 4 ], 3.0 );
	assert.strictEqual( APv[ 5 ], 0.7 );
	assert.strictEqual( APv[ 6 ], 4.0 );
	assert.strictEqual( APv[ 7 ], 0.0 ); // forced real
	assert.strictEqual( APv[ 8 ], 5.0 );
	assert.strictEqual( APv[ 9 ], 0.2 );
	assert.strictEqual( APv[ 10 ], 6.0 );
	assert.strictEqual( APv[ 11 ], 0.0 ); // forced real
});

test( 'zhpr: offset support for x and AP', function t() {
	// Test with non-zero offsets (in complex elements)
	var APv = reinterpret( AP, 0 );
	var AP = new Complex128Array([
		999.0,
		999.0,      // padding (1 complex element)
		3.0,
		0.0            // AP(0,0) = (3,0)
	]);
	var x = new Complex128Array([
		999.0,
		999.0,       // padding (1 complex element)
		2.0,
		1.0            // x(0) = (2,1)
	]);

	// AP(0,0) = real(AP(0,0)) + real(x(0) * 1.5 * conj(x(0)))

	// = 3 + 1.5 * (2^2 + 1^2) = 3 + 7.5 = 10.5
	assert.strictEqual( APv[ 0 ], 999.0 ); // padding untouched
	assert.strictEqual( APv[ 1 ], 999.0 );
	assertClose( APv[ 2 ], 10.5, 1e-14, 'AP[0] real' );
	assert.strictEqual( APv[ 3 ], 0.0 );
});
