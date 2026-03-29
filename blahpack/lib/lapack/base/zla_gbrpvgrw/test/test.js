
'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_gbrpvgrw = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zla_gbrpvgrw.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zla_gbrpvgrw is a function', function t() {
	assert.equal( typeof zla_gbrpvgrw, 'function' );
});

test( 'zla_gbrpvgrw: no_growth', function t() {
	var result;
	var AFB = new Complex128Array( new Float64Array([
		5.0, 0.0, 3.0, 0.0, 7.0, 0.0
	]) );
	var tc = findCase( 'no_growth' );
	var AB = new Complex128Array( new Float64Array([
		5.0, 0.0, 3.0, 0.0, 7.0, 0.0
	]) );

	result = zla_gbrpvgrw( 3, 0, 0, 3, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zla_gbrpvgrw: ncols_zero', function t() {
	var result;
	var AFB = new Complex128Array( new Float64Array([
		5.0, 0.0, 3.0, 0.0, 7.0, 0.0
	]) );
	var tc = findCase( 'ncols_zero' );
	var AB = new Complex128Array( new Float64Array([
		5.0, 0.0, 3.0, 0.0, 7.0, 0.0
	]) );

	result = zla_gbrpvgrw( 3, 0, 0, 0, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zla_gbrpvgrw: single_element_growth', function t() {
	var result;
	var AFB = new Complex128Array( new Float64Array( [ 6.0, 2.0 ] ) );
	var tc = findCase( 'single_element_growth' );
	var AB = new Complex128Array( new Float64Array( [ 3.0, 4.0 ] ) );

	result = zla_gbrpvgrw( 1, 0, 0, 1, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zla_gbrpvgrw: growth_factor', function t() {
	var result;
	var AFB = new Complex128Array( new Float64Array([
		4.0, 2.0, 3.0, 0.0, 5.0, 3.0
	]) );
	var tc = findCase( 'growth_factor' );
	var AB = new Complex128Array( new Float64Array([
		2.0, 1.0, 3.0, 0.0, 1.0, 1.0
	]) );

	result = zla_gbrpvgrw( 3, 0, 0, 3, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zla_gbrpvgrw: zero_umax', function t() {
	var result;
	var AFB = new Complex128Array( new Float64Array([
		5.0, 1.0, 0.0, 0.0, 8.0, 0.0
	]) );
	var tc = findCase( 'zero_umax' );
	var AB = new Complex128Array( new Float64Array([
		5.0, 1.0, 3.0, 2.0, 4.0, 0.0
	]) );

	result = zla_gbrpvgrw( 3, 0, 0, 3, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zla_gbrpvgrw: tridiagonal', function t() {
	var result;
	var AFB = new Complex128Array( new Float64Array([
		// Col 0: rows [0,1,2,3]
		0.0,
		0.0,
		4.0,
		1.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 1: rows [0,1,2,3]
		2.0,
		0.0,
		10.0,
		3.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 2: rows [0,1,2,3]
		1.0,
		1.0,
		6.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 3: rows [0,1,2,3]
		4.0,
		0.0,
		7.0,
		2.0,
		0.0,
		0.0,
		0.0,
		0.0
	]) );
	var tc = findCase( 'tridiagonal' );
	var AB = new Complex128Array( new Float64Array([
		// Col 0: rows [0,1,2]
		0.0,
		0.0,
		4.0,
		1.0,
		1.0,
		0.0,

		// Col 1: rows [0,1,2]
		2.0,
		0.0,
		5.0,
		2.0,
		3.0,
		1.0,

		// Col 2: rows [0,1,2]
		1.0,
		1.0,
		6.0,
		0.0,
		2.0,
		0.0,

		// Col 3: rows [0,1,2]
		4.0,
		0.0,
		3.0,
		1.0,
		0.0,
		0.0
	]) );

	result = zla_gbrpvgrw( 4, 1, 1, 4, AB, 1, 3, 0, AFB, 1, 4, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zla_gbrpvgrw: ncols_less_than_n', function t() {
	var result;
	var AFB = new Complex128Array( new Float64Array([
		6.0, 2.0, 4.0, 0.0, 100.0, 0.0, 100.0, 0.0
	]) );
	var tc = findCase( 'ncols_less_than_n' );
	var AB = new Complex128Array( new Float64Array([
		3.0, 1.0, 4.0, 0.0, 1.0, 0.0, 2.0, 0.0
	]) );

	result = zla_gbrpvgrw( 4, 0, 0, 2, AB, 1, 1, 0, AFB, 1, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zla_gbrpvgrw: wider_band', function t() {
	var result;
	var AFB = new Complex128Array( new Float64Array([
		// Col 0
		0.0,
		0.0,
		0.0,
		0.0,
		8.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 1
		0.0,
		0.0,
		3.0,
		0.0,
		14.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 2
		1.0,
		0.0,
		2.0,
		0.0,
		9.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 3
		5.0,
		0.0,
		1.0,
		0.0,
		12.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 4
		2.0,
		0.0,
		3.0,
		0.0,
		10.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0
	]) );
	var tc = findCase( 'wider_band' );
	var AB = new Complex128Array( new Float64Array([
		// Col 0
		0.0,
		0.0,
		0.0,
		0.0,
		8.0,
		0.0,
		2.0,
		0.0,
		1.0,
		0.0,

		// Col 1
		0.0,
		0.0,
		3.0,
		0.0,
		7.0,
		1.0,
		4.0,
		0.0,
		1.0,
		0.0,

		// Col 2
		1.0,
		0.0,
		2.0,
		0.0,
		9.0,
		0.0,
		3.0,
		0.0,
		2.0,
		0.0,

		// Col 3
		5.0,
		0.0,
		1.0,
		0.0,
		6.0,
		0.0,
		4.0,
		0.0,
		0.0,
		0.0,

		// Col 4
		2.0,
		0.0,
		3.0,
		0.0,
		5.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0
	]) );

	result = zla_gbrpvgrw( 5, 2, 2, 5, AB, 1, 5, 0, AFB, 1, 7, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zla_gbrpvgrw: complex_values', function t() {
	var result;
	var AFB = new Complex128Array( new Float64Array([
		// Col 0: rows [0,1,2,3]
		0.0,
		0.0,
		3.0,
		-4.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 1: rows [0,1,2,3]
		-2.0,
		1.0,
		10.0,
		-5.0,
		0.0,
		0.0,
		0.0,
		0.0,

		// Col 2: rows [0,1,2,3]
		4.0,
		-1.0,
		-2.0,
		-8.0,
		0.0,
		0.0,
		0.0,
		0.0
	]) );
	var tc = findCase( 'complex_values' );
	var AB = new Complex128Array( new Float64Array([
		// Col 0: rows [0,1,2]
		0.0,
		0.0,
		3.0,
		-4.0,
		1.0,
		2.0,

		// Col 1: rows [0,1,2]
		-2.0,
		1.0,
		5.0,
		-3.0,
		0.0,
		6.0,

		// Col 2: rows [0,1,2]
		4.0,
		-1.0,
		-2.0,
		-3.0,
		0.0,
		0.0
	]) );

	result = zla_gbrpvgrw( 3, 1, 1, 3, AB, 1, 3, 0, AFB, 1, 4, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
