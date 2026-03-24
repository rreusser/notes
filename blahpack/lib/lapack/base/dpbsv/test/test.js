'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dpbsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpbsv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dpbsv: main export is a function', function t() {
	assert.strictEqual( typeof dpbsv, 'function' );
});

test( 'dpbsv: upper tridiagonal N=5, NRHS=1', function t() {
	var tc = findCase( 'upper_tridiag_nrhs1' );
	// Upper band storage: LDAB=2, row 0 = superdiag, row 1 = diag
	// Column-major flat: [0, 2, -1, 2, -1, 2, -1, 2, -1, 2]
	var AB = new Float64Array( [
		0, 2,
		-1, 2,
		-1, 2,
		-1, 2,
		-1, 2
	] );
	var B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	var info = dpbsv( 'upper', 5, 1, 1, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbsv: lower tridiagonal N=5, NRHS=1', function t() {
	var tc = findCase( 'lower_tridiag_nrhs1' );
	// Lower band storage: LDAB=2, row 0 = diag, row 1 = subdiag
	var AB = new Float64Array( [
		2, -1,
		2, -1,
		2, -1,
		2, -1,
		2, 0
	] );
	var B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	var info = dpbsv( 'lower', 5, 1, 1, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbsv: upper tridiagonal N=5, NRHS=2', function t() {
	var tc = findCase( 'upper_tridiag_nrhs2' );
	var AB = new Float64Array( [
		0, 2,
		-1, 2,
		-1, 2,
		-1, 2,
		-1, 2
	] );
	// B is N-by-NRHS in column-major: col1 then col2
	var B = new Float64Array( [
		1, 2, 3, 4, 5,
		5, 4, 3, 2, 1
	] );
	var info = dpbsv( 'upper', 5, 1, 2, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbsv: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var AB = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var info = dpbsv( 'upper', 0, 0, 1, AB, 1, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'dpbsv: NRHS=0 quick return', function t() {
	var tc = findCase( 'nrhs_zero' );
	var AB = new Float64Array( [ 4 ] );
	var B = new Float64Array( 1 );
	var info = dpbsv( 'lower', 1, 0, 0, AB, 1, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'dpbsv: N=1', function t() {
	var tc = findCase( 'n_one' );
	var AB = new Float64Array( [ 4 ] );
	var B = new Float64Array( [ 8 ] );
	var info = dpbsv( 'upper', 1, 0, 1, AB, 1, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbsv: not positive definite (INFO > 0)', function t() {
	var tc = findCase( 'not_posdef' );
	var AB = new Float64Array( [
		1, 2,
		0, 0
	] );
	var B = new Float64Array( [ 1, 1 ] );
	var info = dpbsv( 'lower', 2, 1, 1, AB, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'dpbsv: lower pentadiagonal N=4, NRHS=1', function t() {
	var tc = findCase( 'lower_penta_nrhs1' );
	var AB = new Float64Array( [
		6, -1, 0.5,
		6, -1, 0.5,
		6, -1, 0,
		6, 0, 0
	] );
	var B = new Float64Array( [ 1, 2, 3, 4 ] );
	var info = dpbsv( 'lower', 4, 2, 1, AB, 1, 3, 0, B, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbsv: upper pentadiagonal N=4, NRHS=1', function t() {
	var tc = findCase( 'upper_penta_nrhs1' );
	var AB = new Float64Array( [
		0, 0, 6,
		0, -1, 6,
		0.5, -1, 6,
		0.5, -1, 6
	] );
	var B = new Float64Array( [ 1, 2, 3, 4 ] );
	var info = dpbsv( 'upper', 4, 2, 1, AB, 1, 3, 0, B, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( B ), tc.x, 1e-10, 'x' );
});
