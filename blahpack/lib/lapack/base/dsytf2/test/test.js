'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsytf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsytf2.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Converts Fortran 1-based IPIV to 0-based JS IPIV.
* Positive values: subtract 1. Negative values: unchanged (already encodes 0-based via ~kp).
*/
function convertIPIV( fipiv ) {
	var out = [];
	var i;
	for ( i = 0; i < fipiv.length; i++ ) {
		if ( fipiv[ i ] > 0 ) {
			out.push( fipiv[ i ] - 1 );
		} else {
			out.push( fipiv[ i ] );
		}
	}
	return out;
}


// TESTS //

test( 'dsytf2: 4x4_lower', function t() {
	var ipiv = new Int32Array( 4 );
	var tc = findCase( '4x4_lower' );
	var A = new Float64Array([
		2, -1, 0, 0,
		0, 2, -1, 0,
		0, 0, 2, -1,
		0, 0, 0, 2
	]);
	var info = dsytf2( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytf2: 4x4_upper', function t() {
	var ipiv = new Int32Array( 4 );
	var tc = findCase( '4x4_upper' );
	var A = new Float64Array([
		2, 0, 0, 0,
		-1, 2, 0, 0,
		0, -1, 2, 0,
		0, 0, -1, 2
	]);
	var info = dsytf2( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytf2: 4x4_indef_lower', function t() {
	var ipiv = new Int32Array( 4 );
	var tc = findCase( '4x4_indef_lower' );
	var A = new Float64Array([
		0, 1, 2, 3,
		0, 0, 4, 5,
		0, 0, 0, 6,
		0, 0, 0, 0
	]);
	var info = dsytf2( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytf2: 4x4_indef_upper', function t() {
	var ipiv = new Int32Array( 4 );
	var tc = findCase( '4x4_indef_upper' );
	var A = new Float64Array([
		0, 0, 0, 0,
		1, 0, 0, 0,
		2, 4, 0, 0,
		3, 5, 6, 0
	]);
	var info = dsytf2( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytf2: n_zero', function t() {
	var ipiv = new Int32Array( 1 );
	var A = new Float64Array( 1 );
	var info = dsytf2( 'lower', 0, A, 1, 1, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytf2: n_one', function t() {
	var ipiv = new Int32Array( 1 );
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 5.0 ]);
	var info = dsytf2( 'lower', 1, A, 1, 1, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytf2: singular', function t() {
	var ipiv = new Int32Array( 2 );
	var tc = findCase( 'singular' );
	var A = new Float64Array([ 0, 0, 0, 0 ]);
	var info = dsytf2( 'lower', 2, A, 1, 2, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytf2: 5x5_lower', function t() {
	var ipiv = new Int32Array( 5 );
	var tc = findCase( '5x5_lower' );
	var A = new Float64Array([
		1, -2, 0, 3, 1,
		0, 0, 4, -1, 2,
		0, 0, -3, 2, 0,
		0, 0, 0, 1, -2,
		0, 0, 0, 0, 4
	]);
	var info = dsytf2( 'lower', 5, A, 1, 5, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytf2: 5x5_upper', function t() {
	var ipiv = new Int32Array( 5 );
	var tc = findCase( '5x5_upper' );
	// Upper triangle of symmetric matrix, column-major (sa1=1, sa2=5)
	var A = new Float64Array( 25 );
	A[ 0 ] = 1;                                 // (0,0)
	A[ 5 ] = -2; A[ 6 ] = 0;                    // (0,1), (1,1)
	A[ 10 ] = 0; A[ 11 ] = 4; A[ 12 ] = -3;    // (0,2), (1,2), (2,2)
	A[ 15 ] = 3; A[ 16 ] = -1; A[ 17 ] = 2; A[ 18 ] = 1; // (0,3)..(3,3)
	A[ 20 ] = 1; A[ 21 ] = 2; A[ 22 ] = 0; A[ 23 ] = -2; A[ 24 ] = 4;
	var info = dsytf2( 'upper', 5, A, 1, 5, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});
