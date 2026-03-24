'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlaqge = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaqge.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}

// Common input matrix (3x3, column-major)
function inputA() {
	return new Float64Array( [ 2.0, 1.0, 0.5, 1.0, 3.0, 1.0, 0.5, 1.0, 4.0 ] );
}

function inputR() {
	return new Float64Array( [ 0.5, 1.0, 0.8 ] );
}

function inputC() {
	return new Float64Array( [ 0.6, 1.0, 0.7 ] );
}


// TESTS //

test( 'dlaqge: no equilibration (rowcnd >= thresh, colcnd >= thresh, amax in range)', function t() {
	var equed;
	var tc = findCase( 'no_equil' );
	var A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.5, 0.6, 4.0 );
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: row equilibration only (rowcnd < thresh)', function t() {
	var equed;
	var tc = findCase( 'row_equil' );
	var A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.01, 0.6, 4.0 );
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: column equilibration only (colcnd < thresh)', function t() {
	var equed;
	var tc = findCase( 'col_equil' );
	var A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.5, 0.01, 4.0 );
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: both row and column equilibration', function t() {
	var equed;
	var tc = findCase( 'both_equil' );
	var A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.01, 0.01, 4.0 );
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: row scaling triggered by amax > large', function t() {
	var equed;
	var tc = findCase( 'amax_large' );
	var A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.5, 0.6, 1.0e300 );
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: row scaling triggered by amax < small', function t() {
	var equed;
	var tc = findCase( 'amax_small' );
	var A = inputA();
	equed = dlaqge( 3, 3, A, 1, 3, 0, inputR(), 1, 0, inputC(), 1, 0, 0.5, 0.6, 1.0e-320 );
	assert.equal( equed, tc.equed );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlaqge: quick return M=0', function t() {
	var equed = dlaqge( 0, 3, new Float64Array( 0 ), 1, 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 3 ), 1, 0, 0.5, 0.6, 4.0 );
	assert.equal( equed, 'none' );
});

test( 'dlaqge: quick return N=0', function t() {
	var equed = dlaqge( 3, 0, new Float64Array( 0 ), 1, 1, 0, new Float64Array( 3 ), 1, 0, new Float64Array( 0 ), 1, 0, 0.5, 0.6, 4.0 );
	assert.equal( equed, 'none' );
});
