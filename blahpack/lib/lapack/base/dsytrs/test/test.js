'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsytrf = require( '../../dsytrf/lib/base.js' );
var dsytrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsytrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dsytrs: 4x4_lower_1rhs', function t() {
	var tc = findCase( '4x4_lower_1rhs' );
	var A = new Float64Array([
		4, 2, 1, 0,
		0, 5, 2, 1,
		0, 0, 6, 3,
		0, 0, 0, 8
	]);
	var b = new Float64Array([ 7, 10, 12, 12 ]);
	var ipiv = new Int32Array( 4 );
	dsytrf( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	var info = dsytrs( 'lower', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 4x4_upper_1rhs', function t() {
	var tc = findCase( '4x4_upper_1rhs' );
	var A = new Float64Array( 16 );
	A[ 0 ] = 4;
	A[ 4 ] = 2; A[ 5 ] = 5;
	A[ 8 ] = 1; A[ 9 ] = 2; A[ 10 ] = 6;
	A[ 12 ] = 0; A[ 13 ] = 1; A[ 14 ] = 3; A[ 15 ] = 8;
	var b = new Float64Array([ 7, 10, 12, 12 ]);
	var ipiv = new Int32Array( 4 );
	dsytrf( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	var info = dsytrs( 'upper', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 4x4_indef_lower_1rhs', function t() {
	var tc = findCase( '4x4_indef_lower_1rhs' );
	var A = new Float64Array([
		0, 1, 2, 3,
		0, 0, 4, 5,
		0, 0, 0, 6,
		0, 0, 0, 0
	]);
	var b = new Float64Array([ 6, 10, 12, 14 ]);
	var ipiv = new Int32Array( 4 );
	dsytrf( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	var info = dsytrs( 'lower', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 4x4_indef_upper_1rhs', function t() {
	var tc = findCase( '4x4_indef_upper_1rhs' );
	var A = new Float64Array( 16 );
	A[ 0 ] = 0;
	A[ 4 ] = 1; A[ 5 ] = 0;
	A[ 8 ] = 2; A[ 9 ] = 4; A[ 10 ] = 0;
	A[ 12 ] = 3; A[ 13 ] = 5; A[ 14 ] = 6; A[ 15 ] = 0;
	var b = new Float64Array([ 6, 10, 12, 14 ]);
	var ipiv = new Int32Array( 4 );
	dsytrf( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	var info = dsytrs( 'upper', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 3x3_lower_2rhs', function t() {
	var tc = findCase( '3x3_lower_2rhs' );
	var A = new Float64Array([
		4, 2, 1,
		0, 5, 2,
		0, 0, 6
	]);
	// B is 3x2, column-major: B(:,1) = [7;9;9], B(:,2) = [14;18;18]
	var b = new Float64Array([ 7, 9, 9, 14, 18, 18 ]);
	var ipiv = new Int32Array( 3 );
	dsytrf( 'lower', 3, A, 1, 3, 0, ipiv, 1, 0 );
	var info = dsytrs( 'lower', 3, 2, A, 1, 3, 0, ipiv, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: n_zero', function t() {
	var A = new Float64Array( 1 );
	var b = new Float64Array( 1 );
	var ipiv = new Int32Array( 1 );
	var info = dsytrs( 'lower', 0, 1, A, 1, 1, 0, ipiv, 1, 0, b, 1, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytrs: nrhs_zero', function t() {
	var A = new Float64Array( 9 );
	var b = new Float64Array( 3 );
	var ipiv = new Int32Array( 3 );
	var info = dsytrs( 'lower', 3, 0, A, 1, 3, 0, ipiv, 1, 0, b, 1, 3, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytrs: n_one_lower', function t() {
	var tc = findCase( 'n_one_lower' );
	var A = new Float64Array([ 4.0 ]);
	var b = new Float64Array([ 8.0 ]);
	var ipiv = new Int32Array([ 0 ]); // 0-based self-pivot
	var info = dsytrs( 'lower', 1, 1, A, 1, 1, 0, ipiv, 1, 0, b, 1, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-14, 'b' );
});

test( 'dsytrs: 5x5_lower_solve', function t() {
	var tc = findCase( '5x5_lower_solve' );
	// Factor then solve: A*x = b where x = [1;2;3;4;5]
	var A = new Float64Array([
		1, -2, 0, 3, 1,
		0, 0, 4, -1, 2,
		0, 0, -3, 2, 0,
		0, 0, 0, 1, -2,
		0, 0, 0, 0, 4
	]);
	var b = new Float64Array([ 14, 16, 7, 1, 17 ]);
	var ipiv = new Int32Array( 5 );
	dsytrf( 'lower', 5, A, 1, 5, 0, ipiv, 1, 0 );
	var info = dsytrs( 'lower', 5, 1, A, 1, 5, 0, ipiv, 1, 0, b, 1, 5, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs: 5x5_upper_solve', function t() {
	var tc = findCase( '5x5_upper_solve' );
	var A = new Float64Array( 25 );
	A[ 0 ] = 1;
	A[ 5 ] = -2; A[ 6 ] = 0;
	A[ 10 ] = 0; A[ 11 ] = 4; A[ 12 ] = -3;
	A[ 15 ] = 3; A[ 16 ] = -1; A[ 17 ] = 2; A[ 18 ] = 1;
	A[ 20 ] = 1; A[ 21 ] = 2; A[ 22 ] = 0; A[ 23 ] = -2; A[ 24 ] = 4;
	var b = new Float64Array([ 14, 16, 7, 1, 17 ]);
	var ipiv = new Int32Array( 5 );
	dsytrf( 'upper', 5, A, 1, 5, 0, ipiv, 1, 0 );
	var info = dsytrs( 'upper', 5, 1, A, 1, 5, 0, ipiv, 1, 0, b, 1, 5, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});
