/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dgeqlf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgeqlf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// INPUT DATA (column-major, mirrors the Fortran test fixtures) //

var INPUT_3X3 = [ 2, 1, 3, 1, 4, 2, 3, 2, 5 ];
var INPUT_4X3 = [ 2, 1, 3, 1, 1, 4, 2, 3, 3, 2, 5, 1 ];
var INPUT_3X4 = [ 2, 1, 3, 1, 4, 2, 3, 2, 5, 4, 1, 2 ];


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

function buildLarge( M, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			if ( i === j ) {
				out.push( 10.0 );
			} else {
				out.push( 1.0 / ( Math.abs( i - j ) + 1 ) );
			}
		}
	}
	return out;
}

function runCase( tc, M, N, input, tol ) {
	var A = new Float64Array( input );
	var TAU = new Float64Array( Math.min( M, N ) );
	var WORK = new Float64Array( Math.max( 1, N ) );
	var info = dgeqlf( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( A, tc.A, tol, 'A' );
	assertArrayClose( TAU, tc.TAU, tol, 'TAU' );
}


// TESTS //

test( 'dgeqlf: 3x3', function t() {
	runCase( findCase( '3x3' ), 3, 3, INPUT_3X3, 1e-13 );
});

test( 'dgeqlf: 4x3', function t() {
	runCase( findCase( '4x3' ), 4, 3, INPUT_4X3, 1e-13 );
});

test( 'dgeqlf: 3x4', function t() {
	runCase( findCase( '3x4' ), 3, 4, INPUT_3X4, 1e-13 );
});

test( 'dgeqlf: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dgeqlf( 3, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );
	assert.equal( info, tc.INFO, 'INFO' );
});

test( 'dgeqlf: large_150x150 (exercises blocked path)', function t() {
	runCase( findCase( 'large_150x150' ), 150, 150, buildLarge( 150, 150 ), 1e-10 );
});

test( 'dgeqlf: m_zero', function t() {
	var A = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dgeqlf( 0, 3, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );
	assert.equal( info, 0, 'INFO' );
});

test( 'dgeqlf: allocates internal workspace when none provided', function t() {
	var tc = findCase( '3x3' );
	var A = new Float64Array( INPUT_3X3 );
	var TAU = new Float64Array( 3 );
	var info = dgeqlf( 3, 3, A, 1, 3, 0, TAU, 1, 0, null, 1, 0, 0 );
	assert.equal( info, 0, 'INFO' );
	assertArrayClose( A, tc.A, 1e-13, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-13, 'TAU' );
});
