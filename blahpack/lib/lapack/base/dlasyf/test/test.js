'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlasyf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlasyf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function convertIPIV( fipiv ) {
	var out = [];
	var i;
	for ( i = 0; i < fipiv.length; i++ ) {
		if ( fipiv[ i ] > 0 ) {
			out.push( fipiv[ i ] - 1 );
		} else if ( fipiv[ i ] < 0 ) {
			out.push( fipiv[ i ] );
		} else {
			out.push( 0 );
		}
	}
	return out;
}


// TESTS //

test( 'dlasyf: 6x6_lower_nb3', function t() {
	var tc = findCase( '6x6_lower_nb3' );
	var A = new Float64Array( 36 );
	A[ 0 ] = 2; A[ 1 ] = -1; A[ 2 ] = 0; A[ 3 ] = 1; A[ 4 ] = 0; A[ 5 ] = 0;
	A[ 7 ] = 3; A[ 8 ] = -1; A[ 9 ] = 0; A[ 10 ] = 1; A[ 11 ] = 0;
	A[ 14 ] = 2; A[ 15 ] = -1; A[ 16 ] = 0; A[ 17 ] = 1;
	A[ 21 ] = 4; A[ 22 ] = -1; A[ 23 ] = 0;
	A[ 28 ] = 3; A[ 29 ] = -1;
	A[ 35 ] = 2;
	var ipiv = new Int32Array( 6 );
	var W = new Float64Array( 36 );
	var result = dlasyf( 'L', 6, 3, A, 1, 6, 0, ipiv, 1, 0, W, 1, 6, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.kb, tc.kb, 'kb' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dlasyf: 6x6_upper_nb3', function t() {
	var tc = findCase( '6x6_upper_nb3' );
	var A = new Float64Array( 36 );
	A[ 0 ] = 2;
	A[ 6 ] = -1; A[ 7 ] = 3;
	A[ 13 ] = -1; A[ 14 ] = 2;
	A[ 18 ] = 1; A[ 21 ] = 4;
	A[ 25 ] = 1; A[ 27 ] = -1; A[ 28 ] = 3;
	A[ 32 ] = 1; A[ 34 ] = -1; A[ 35 ] = 2;
	var ipiv = new Int32Array( 6 );
	var W = new Float64Array( 36 );
	var result = dlasyf( 'U', 6, 3, A, 1, 6, 0, ipiv, 1, 0, W, 1, 6, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.kb, tc.kb, 'kb' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dlasyf: 6x6_indef_lower_nb3', function t() {
	var tc = findCase( '6x6_indef_lower_nb3' );
	var A = new Float64Array( 36 );
	A[ 0 ] = 0; A[ 1 ] = 3; A[ 2 ] = 1; A[ 3 ] = 2; A[ 4 ] = 0; A[ 5 ] = 1;
	A[ 7 ] = 0; A[ 8 ] = 2; A[ 9 ] = 1; A[ 10 ] = 3; A[ 11 ] = 0;
	A[ 14 ] = 0; A[ 15 ] = 1; A[ 16 ] = 2; A[ 17 ] = 3;
	A[ 21 ] = 5; A[ 22 ] = 1; A[ 23 ] = 2;
	A[ 28 ] = 0; A[ 29 ] = 1;
	A[ 35 ] = 0;
	var ipiv = new Int32Array( 6 );
	var W = new Float64Array( 36 );
	var result = dlasyf( 'L', 6, 3, A, 1, 6, 0, ipiv, 1, 0, W, 1, 6, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.kb, tc.kb, 'kb' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dlasyf: 6x6_indef_upper_nb3', function t() {
	var tc = findCase( '6x6_indef_upper_nb3' );
	var A = new Float64Array( 36 );
	A[ 0 ] = 0;
	A[ 6 ] = 3; A[ 7 ] = 0;
	A[ 12 ] = 1; A[ 13 ] = 2; A[ 14 ] = 0;
	A[ 18 ] = 2; A[ 19 ] = 1; A[ 20 ] = 1; A[ 21 ] = 5;
	A[ 24 ] = 0; A[ 25 ] = 3; A[ 26 ] = 2; A[ 27 ] = 1; A[ 28 ] = 0;
	A[ 30 ] = 1; A[ 31 ] = 0; A[ 32 ] = 3; A[ 33 ] = 2; A[ 34 ] = 1; A[ 35 ] = 0;
	var ipiv = new Int32Array( 6 );
	var W = new Float64Array( 36 );
	var result = dlasyf( 'U', 6, 3, A, 1, 6, 0, ipiv, 1, 0, W, 1, 6, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.kb, tc.kb, 'kb' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});
