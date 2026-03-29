

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dla_gbrpvgrw = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dla_gbrpvgrw.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// TESTS //

test( 'dla_gbrpvgrw is a function', function t() {
	assert.equal( typeof dla_gbrpvgrw, 'function' );
});

test( 'dla_gbrpvgrw: no_growth', function t() {
	var tc = findCase( 'no_growth' );
	var result;

	// kl=0, ku=0, n=3, ncols=3, ldab=1, ldafb=1
	// Diagonal: AB(1,j) = AFB(1,j)
	var AB = new Float64Array( [ 5.0, 3.0, 7.0 ] );
	var AFB = new Float64Array( [ 5.0, 3.0, 7.0 ] );

	result = dla_gbrpvgrw( 3, 0, 0, 3, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: ncols_zero', function t() {
	var tc = findCase( 'ncols_zero' );
	var result;

	var AB = new Float64Array( [ 5.0, 3.0, 7.0 ] );
	var AFB = new Float64Array( [ 5.0, 3.0, 7.0 ] );

	result = dla_gbrpvgrw( 3, 0, 0, 0, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: single_element_growth', function t() {
	var tc = findCase( 'single_element_growth' );
	var result;

	// kl=0, ku=0, n=1, ncols=1
	var AB = new Float64Array( [ 3.0 ] );
	var AFB = new Float64Array( [ 6.0 ] );

	result = dla_gbrpvgrw( 1, 0, 0, 1, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: growth_factor', function t() {
	var tc = findCase( 'growth_factor' );
	var result;

	// kl=0, ku=0, n=3, ncols=3
	// Col 1: 2/4=0.5, Col 2: 3/3=1.0, Col 3: 1/5=0.2 -> RPVGRW=0.2
	var AB = new Float64Array( [ 2.0, 3.0, 1.0 ] );
	var AFB = new Float64Array( [ 4.0, 3.0, 5.0 ] );

	result = dla_gbrpvgrw( 3, 0, 0, 3, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: zero_umax', function t() {
	var tc = findCase( 'zero_umax' );
	var result;

	// kl=0, ku=0, n=3, ncols=3
	// Col 2 UMAX=0 (skipped)
	var AB = new Float64Array( [ 5.0, 3.0, 4.0 ] );
	var AFB = new Float64Array( [ 5.0, 0.0, 8.0 ] );

	result = dla_gbrpvgrw( 3, 0, 0, 3, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: tridiagonal', function t() {
	var tc = findCase( 'tridiagonal' );
	var result;

	// kl=1, ku=1, n=4, ncols=4, ldab=3, ldafb=4, KD=2
	// Column-major band storage: AB is 3-by-4, AFB is 4-by-4
	// strideAB1=1, strideAB2=3 (ldab), strideAFB1=1, strideAFB2=4 (ldafb)

	// AB in column-major flat layout (3 rows x 4 cols)
	var AB = new Float64Array( [
		// Col 0: rows [0,1,2]
		0.0, 4.0, 1.0,
		// Col 1: rows [0,1,2]
		2.0, 5.0, 3.0,
		// Col 2: rows [0,1,2]
		1.0, 6.0, 2.0,
		// Col 3: rows [0,1,2]
		4.0, 3.0, 0.0
	] );

	// AFB in column-major flat layout (4 rows x 4 cols)
	var AFB = new Float64Array( [
		// Col 0: rows [0,1,2,3]
		0.0, 4.0, 0.0, 0.0,
		// Col 1: rows [0,1,2,3]
		2.0, 10.0, 0.0, 0.0,
		// Col 2: rows [0,1,2,3]
		1.0, 6.0, 0.0, 0.0,
		// Col 3: rows [0,1,2,3]
		4.0, 7.0, 0.0, 0.0
	] );

	result = dla_gbrpvgrw( 4, 1, 1, 4, AB, 1, 3, 0, AFB, 1, 4, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: ncols_less_than_n', function t() {
	var tc = findCase( 'ncols_less_than_n' );
	var result;

	// kl=0, ku=0, n=4, ncols=2
	var AB = new Float64Array( [ 3.0, 4.0, 1.0, 2.0 ] );
	var AFB = new Float64Array( [ 6.0, 4.0, 100.0, 100.0 ] );

	result = dla_gbrpvgrw( 4, 0, 0, 2, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_gbrpvgrw: wider_band', function t() {
	var tc = findCase( 'wider_band' );
	var result;

	// kl=2, ku=2, n=5, ncols=5, ldab=5, ldafb=7, KD=3
	// AB is 5-by-5, AFB is 7-by-5 in column-major

	// AB: column-major, 5 rows per column
	var AB = new Float64Array( [
		// Col 0: i=1(row2), i=2(row3), i=3(row4)
		0.0, 0.0, 8.0, 2.0, 1.0,
		// Col 1: i=1(row1), i=2(row2), i=3(row3), i=4(row4)
		0.0, 3.0, 7.0, 4.0, 1.0,
		// Col 2: i=1(row0), i=2(row1), i=3(row2), i=4(row3), i=5(row4)
		1.0, 2.0, 9.0, 3.0, 2.0,
		// Col 3: i=2(row0), i=3(row1), i=4(row2), i=5(row3)
		5.0, 1.0, 6.0, 4.0, 0.0,
		// Col 4: i=3(row0), i=4(row1), i=5(row2)
		2.0, 3.0, 5.0, 0.0, 0.0
	] );

	// AFB: column-major, 7 rows per column
	var AFB = new Float64Array( [
		// Col 0
		0.0, 0.0, 8.0, 0.0, 0.0, 0.0, 0.0,
		// Col 1
		0.0, 3.0, 14.0, 0.0, 0.0, 0.0, 0.0,
		// Col 2
		1.0, 2.0, 9.0, 0.0, 0.0, 0.0, 0.0,
		// Col 3
		5.0, 1.0, 12.0, 0.0, 0.0, 0.0, 0.0,
		// Col 4
		2.0, 3.0, 10.0, 0.0, 0.0, 0.0, 0.0
	] );

	result = dla_gbrpvgrw( 5, 2, 2, 5, AB, 1, 5, 0, AFB, 1, 7, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
