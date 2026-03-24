

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsytrf = require( '../../dsytrf/lib/base.js' );
var dsytrs2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsytrs2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dsytrs2: 4x4_lower_1rhs', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( '4x4_lower_1rhs' );
	A = new Float64Array([
		4, 2, 1, 0,
		0, 5, 2, 1,
		0, 0, 6, 3,
		0, 0, 0, 8
	]);
	b = new Float64Array([ 7, 10, 12, 12 ]);
	ipiv = new Int32Array( 4 );
	work = new Float64Array( 4 );
	dsytrf( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	info = dsytrs2( 'L', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs2: 4x4_upper_1rhs', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( '4x4_upper_1rhs' );
	A = new Float64Array( 16 );
	A[ 0 ] = 4;
	A[ 4 ] = 2; A[ 5 ] = 5;
	A[ 8 ] = 1; A[ 9 ] = 2; A[ 10 ] = 6;
	A[ 12 ] = 0; A[ 13 ] = 1; A[ 14 ] = 3; A[ 15 ] = 8;
	b = new Float64Array([ 7, 10, 12, 12 ]);
	ipiv = new Int32Array( 4 );
	work = new Float64Array( 4 );
	dsytrf( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	info = dsytrs2( 'U', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs2: 4x4_indef_lower_1rhs', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( '4x4_indef_lower_1rhs' );
	A = new Float64Array([
		0, 1, 2, 3,
		0, 0, 4, 5,
		0, 0, 0, 6,
		0, 0, 0, 0
	]);
	b = new Float64Array([ 6, 10, 12, 14 ]);
	ipiv = new Int32Array( 4 );
	work = new Float64Array( 4 );
	dsytrf( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	info = dsytrs2( 'L', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs2: 4x4_indef_upper_1rhs', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( '4x4_indef_upper_1rhs' );
	A = new Float64Array( 16 );
	A[ 0 ] = 0;
	A[ 4 ] = 1; A[ 5 ] = 0;
	A[ 8 ] = 2; A[ 9 ] = 4; A[ 10 ] = 0;
	A[ 12 ] = 3; A[ 13 ] = 5; A[ 14 ] = 6; A[ 15 ] = 0;
	b = new Float64Array([ 6, 10, 12, 14 ]);
	ipiv = new Int32Array( 4 );
	work = new Float64Array( 4 );
	dsytrf( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	info = dsytrs2( 'U', 4, 1, A, 1, 4, 0, ipiv, 1, 0, b, 1, 4, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs2: 3x3_lower_2rhs', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( '3x3_lower_2rhs' );
	A = new Float64Array([
		4, 2, 1,
		0, 5, 2,
		0, 0, 6
	]);
	b = new Float64Array([
		7, 9, 9,
		14, 18, 18
	]);
	ipiv = new Int32Array( 3 );
	work = new Float64Array( 3 );
	dsytrf( 'lower', 3, A, 1, 3, 0, ipiv, 1, 0 );
	info = dsytrs2( 'L', 3, 2, A, 1, 3, 0, ipiv, 1, 0, b, 1, 3, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs2: 3x3_upper_2rhs', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( '3x3_upper_2rhs' );
	A = new Float64Array( 9 );
	A[ 0 ] = 4;
	A[ 3 ] = 2; A[ 4 ] = 5;
	A[ 6 ] = 1; A[ 7 ] = 2; A[ 8 ] = 6;
	b = new Float64Array([
		7, 9, 9,
		14, 18, 18
	]);
	ipiv = new Int32Array( 3 );
	work = new Float64Array( 3 );
	dsytrf( 'upper', 3, A, 1, 3, 0, ipiv, 1, 0 );
	info = dsytrs2( 'U', 3, 2, A, 1, 3, 0, ipiv, 1, 0, b, 1, 3, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs2: n_zero', function t() {
	var work;
	var ipiv;
	var info;
	var A;
	var b;

	A = new Float64Array( 1 );
	b = new Float64Array( 1 );
	ipiv = new Int32Array( 1 );
	work = new Float64Array( 1 );
	info = dsytrs2( 'L', 0, 1, A, 1, 1, 0, ipiv, 1, 0, b, 1, 1, 0, work, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytrs2: nrhs_zero', function t() {
	var work;
	var ipiv;
	var info;
	var A;
	var b;

	A = new Float64Array( 9 );
	b = new Float64Array( 3 );
	ipiv = new Int32Array( 3 );
	work = new Float64Array( 3 );
	info = dsytrs2( 'L', 3, 0, A, 1, 3, 0, ipiv, 1, 0, b, 1, 3, 0, work, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytrs2: n_one_lower', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( 'n_one_lower' );
	A = new Float64Array([ 4 ]);
	b = new Float64Array([ 8 ]);
	ipiv = new Int32Array( 1 );
	work = new Float64Array( 1 );
	dsytrf( 'lower', 1, A, 1, 1, 0, ipiv, 1, 0 );
	info = dsytrs2( 'L', 1, 1, A, 1, 1, 0, ipiv, 1, 0, b, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs2: n_one_upper', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( 'n_one_upper' );
	A = new Float64Array([ 4 ]);
	b = new Float64Array([ 8 ]);
	ipiv = new Int32Array( 1 );
	work = new Float64Array( 1 );
	dsytrf( 'upper', 1, A, 1, 1, 0, ipiv, 1, 0 );
	info = dsytrs2( 'U', 1, 1, A, 1, 1, 0, ipiv, 1, 0, b, 1, 1, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs2: 5x5_lower_solve', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( '5x5_lower_solve' );
	A = new Float64Array( 25 );
	A[ 0 ] = 1; A[ 1 ] = -2; A[ 2 ] = 0; A[ 3 ] = 3; A[ 4 ] = 1;
	A[ 5 ] = 0; A[ 6 ] = 0; A[ 7 ] = 4; A[ 8 ] = -1; A[ 9 ] = 2;
	A[ 10 ] = 0; A[ 11 ] = 0; A[ 12 ] = -3; A[ 13 ] = 2; A[ 14 ] = 0;
	A[ 15 ] = 0; A[ 16 ] = 0; A[ 17 ] = 0; A[ 18 ] = 1; A[ 19 ] = -2;
	A[ 20 ] = 0; A[ 21 ] = 0; A[ 22 ] = 0; A[ 23 ] = 0; A[ 24 ] = 4;
	b = new Float64Array([ 14, 16, 7, 1, 17 ]);
	ipiv = new Int32Array( 5 );
	work = new Float64Array( 5 );
	dsytrf( 'lower', 5, A, 1, 5, 0, ipiv, 1, 0 );
	info = dsytrs2( 'L', 5, 1, A, 1, 5, 0, ipiv, 1, 0, b, 1, 5, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});

test( 'dsytrs2: 5x5_upper_solve', function t() {
	var work;
	var ipiv;
	var info;
	var tc;
	var A;
	var b;

	tc = findCase( '5x5_upper_solve' );
	A = new Float64Array( 25 );
	A[ 0 ] = 1;
	A[ 5 ] = -2; A[ 6 ] = 0;
	A[ 10 ] = 0; A[ 11 ] = 4; A[ 12 ] = -3;
	A[ 15 ] = 3; A[ 16 ] = -1; A[ 17 ] = 2; A[ 18 ] = 1;
	A[ 20 ] = 1; A[ 21 ] = 2; A[ 22 ] = 0; A[ 23 ] = -2; A[ 24 ] = 4;
	b = new Float64Array([ 14, 16, 7, 1, 17 ]);
	ipiv = new Int32Array( 5 );
	work = new Float64Array( 5 );
	dsytrf( 'upper', 5, A, 1, 5, 0, ipiv, 1, 0 );
	info = dsytrs2( 'U', 5, 1, A, 1, 5, 0, ipiv, 1, 0, b, 1, 5, 0, work, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, tc.b, 1e-12, 'b' );
});
