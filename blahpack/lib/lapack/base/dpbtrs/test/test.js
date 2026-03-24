'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dpbtrf = require( './../../dpbtrf/lib/base.js' );
var dpbtrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpbtrs.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dpbtrs: main export is a function', function t() {
	assert.strictEqual( typeof dpbtrs, 'function' );
});

test( 'dpbtrs: upper tridiagonal N=5, NRHS=1', function t() {
	var tc = findCase( 'upper_tridiag_nrhs1' );
	// Upper band: LDAB=2, KD=1
	var AB = new Float64Array( [
		0, 2, -1, 2, -1, 2, -1, 2, -1, 2
	] );
	// Factorize first
	dpbtrf( 'upper', 5, 1, AB, 1, 2, 0 );
	var B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	var info = dpbtrs( 'upper', 5, 1, 1, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: lower tridiagonal N=5, NRHS=1', function t() {
	var tc = findCase( 'lower_tridiag_nrhs1' );
	var AB = new Float64Array( [
		2, -1, 2, -1, 2, -1, 2, -1, 2, 0
	] );
	dpbtrf( 'lower', 5, 1, AB, 1, 2, 0 );
	var B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	var info = dpbtrs( 'lower', 5, 1, 1, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: upper tridiagonal N=5, NRHS=2', function t() {
	var tc = findCase( 'upper_tridiag_nrhs2' );
	var AB = new Float64Array( [
		0, 2, -1, 2, -1, 2, -1, 2, -1, 2
	] );
	dpbtrf( 'upper', 5, 1, AB, 1, 2, 0 );
	var B = new Float64Array( [
		1, 2, 3, 4, 5,
		5, 4, 3, 2, 1
	] );
	var info = dpbtrs( 'upper', 5, 1, 2, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var AB = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var info = dpbtrs( 'upper', 0, 0, 1, AB, 1, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'dpbtrs: NRHS=0 quick return', function t() {
	var tc = findCase( 'nrhs_zero' );
	var AB = new Float64Array( [ 2 ] );
	var B = new Float64Array( 1 );
	var info = dpbtrs( 'lower', 5, 1, 0, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'dpbtrs: N=1', function t() {
	var tc = findCase( 'n_one' );
	// Already factored: diagonal = 2 (since Fortran set ab(1)=2 directly)
	var AB = new Float64Array( [ 2 ] );
	var B = new Float64Array( [ 6 ] );
	var info = dpbtrs( 'upper', 1, 0, 1, AB, 1, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: upper pentadiagonal N=4, NRHS=1', function t() {
	var tc = findCase( 'upper_penta_nrhs1' );
	var AB = new Float64Array( [
		0, 0, 6,
		0, -1, 6,
		0.5, -1, 6,
		0.5, -1, 6
	] );
	dpbtrf( 'upper', 4, 2, AB, 1, 3, 0 );
	var B = new Float64Array( [ 1, 2, 3, 4 ] );
	var info = dpbtrs( 'upper', 4, 2, 1, AB, 1, 3, 0, B, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: lower pentadiagonal N=4, NRHS=1', function t() {
	var tc = findCase( 'lower_penta_nrhs1' );
	var AB = new Float64Array( [
		6, -1, 0.5,
		6, -1, 0.5,
		6, -1, 0,
		6, 0, 0
	] );
	dpbtrf( 'lower', 4, 2, AB, 1, 3, 0 );
	var B = new Float64Array( [ 1, 2, 3, 4 ] );
	var info = dpbtrs( 'lower', 4, 2, 1, AB, 1, 3, 0, B, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: lower pentadiagonal N=4, NRHS=3', function t() {
	var tc = findCase( 'lower_penta_nrhs3' );
	var AB = new Float64Array( [
		6, -1, 0.5,
		6, -1, 0.5,
		6, -1, 0,
		6, 0, 0
	] );
	dpbtrf( 'lower', 4, 2, AB, 1, 3, 0 );
	// 3 RHS columns in B (ldb=4)
	var B = new Float64Array( [
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0
	] );
	var info = dpbtrs( 'lower', 4, 2, 3, AB, 1, 3, 0, B, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});
