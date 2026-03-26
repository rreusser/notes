'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlagtf = require( '../../dlagtf/lib/base.js' );
var dlagts = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlagts.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function setupFactorization5() {
	var a = new Float64Array( [ 4.0, 4.0, 4.0, 4.0, 4.0 ] );
	var b = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var c = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var d = new Float64Array( 3 );
	var IN = new Int32Array( 5 );
	var tol = 0.0;

	dlagtf( 5, a, 1, 0, 1.0, b, 1, 0, c, 1, 0, tol, d, 1, 0, IN, 1, 0 );
	return { a: a, b: b, c: c, d: d, IN: IN };
}

function setupFactorization2() {
	var a = new Float64Array( [ 5.0, 3.0 ] );
	var b = new Float64Array( [ 2.0 ] );
	var c = new Float64Array( [ 0.5 ] );
	var d = new Float64Array( 1 );
	var IN = new Int32Array( 2 );
	var tol = 0.0;

	dlagtf( 2, a, 1, 0, 0.0, b, 1, 0, c, 1, 0, tol, d, 1, 0, IN, 1, 0 );
	return { a: a, b: b, c: c, d: d, IN: IN };
}

function setupFactorization1() {
	var a = new Float64Array( [ 3.0 ] );
	var b = new Float64Array( 1 );
	var c = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var IN = new Int32Array( 1 );
	var tol = 0.0;

	dlagtf( 1, a, 1, 0, 0.5, b, 1, 0, c, 1, 0, tol, d, 1, 0, IN, 1, 0 );
	return { a: a, b: b, c: c, d: d, IN: IN };
}


// TESTS //

test( 'dlagts: solve (T-lambda*I)x=y with perturbation (job=-1)', function t() {
	var tc = findCase( 'solve_job_m1' );
	var f = setupFactorization5();
	var y = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );

	var info = dlagts( -1, 5, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlagts: solve transpose with perturbation (job=-2)', function t() {
	var tc = findCase( 'solve_job_m2' );
	var f = setupFactorization5();
	var y = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );

	var info = dlagts( -2, 5, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlagts: N=0', function t() {
	var f = setupFactorization5();
	var y = new Float64Array( 1 );

	var info = dlagts( -1, 0, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );

	assert.equal( info, 0, 'info' );
});

test( 'dlagts: solve without perturbation (job=1)', function t() {
	var tc = findCase( 'solve_job_1' );
	var f = setupFactorization5();
	var y = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );

	var info = dlagts( 1, 5, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlagts: solve transpose without perturbation (job=2)', function t() {
	var tc = findCase( 'solve_job_2' );
	var f = setupFactorization5();
	var y = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );

	var info = dlagts( 2, 5, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlagts: N=1', function t() {
	var tc = findCase( 'n_equals_1' );
	var f = setupFactorization1();
	var y = new Float64Array( [ 7.0 ] );

	var info = dlagts( -1, 1, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlagts: job=1 N=2', function t() {
	var tc = findCase( 'solve_job_1_n2' );
	var f = setupFactorization2();
	var y = new Float64Array( [ 1.0, 2.0 ] );

	var info = dlagts( 1, 2, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlagts: job=2 N=2', function t() {
	var tc = findCase( 'solve_job_2_n2' );
	var f = setupFactorization2();
	var y = new Float64Array( [ 1.0, 2.0 ] );

	var info = dlagts( 2, 2, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlagts: job=-2 N=2', function t() {
	var tc = findCase( 'solve_job_m2_n2' );
	var f = setupFactorization2();
	var y = new Float64Array( [ 1.0, 2.0 ] );

	var info = dlagts( -2, 2, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlagts: job=-1 with explicit tol', function t() {
	var tc = findCase( 'solve_job_m1_explicit_tol' );
	var f = setupFactorization5();
	var y = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );

	var info = dlagts( -1, 5, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 1e-8 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlagts: invalid job returns -1', function t() {
	var f = setupFactorization5();
	var y = new Float64Array( [ 1.0 ] );
	var info = dlagts( 0, 5, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );
	assert.equal( info, -1, 'info' );

	info = dlagts( 3, 5, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );
	assert.equal( info, -1, 'info for job=3' );
});

test( 'dlagts: negative N returns -2', function t() {
	var f = setupFactorization5();
	var y = new Float64Array( [ 1.0 ] );
	var info = dlagts( 1, -1, f.a, 1, 0, f.b, 1, 0, f.c, 1, 0, f.d, 1, 0, f.IN, 1, 0, y, 1, 0, 0.0 );
	assert.equal( info, -2, 'info' );
});
