

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dgeqlf = require( './../lib/dgeqlf.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgeqlf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dgeqlf is a function', function t() {
	assert.strictEqual( typeof dgeqlf, 'function', 'is a function' );
});

test( 'dgeqlf has expected arity', function t() {
	assert.strictEqual( dgeqlf.length, 10, 'has expected arity' );
});

test( 'dgeqlf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgeqlf( 'invalid', 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'dgeqlf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgeqlf( 'row-major', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});

test( 'dgeqlf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgeqlf( 'row-major', 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});

test( 'dgeqlf throws RangeError for row-major LDA < N', function t() {
	assert.throws( function throws() {
		dgeqlf( 'row-major', 3, 5, new Float64Array( 25 ), 3, new Float64Array( 3 ), 1, new Float64Array( 5 ), 1, 5 );
	}, RangeError );
});

test( 'dgeqlf throws RangeError for column-major LDA < M', function t() {
	assert.throws( function throws() {
		dgeqlf( 'column-major', 5, 3, new Float64Array( 25 ), 3, new Float64Array( 3 ), 1, new Float64Array( 5 ), 1, 5 );
	}, RangeError );
});

test( 'dgeqlf column-major 3x3 matches fixture', function t() {
	var tc = findCase( '3x3' );
	var A = new Float64Array( [ 2, 1, 3, 1, 4, 2, 3, 2, 5 ] );
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 );
	var info = dgeqlf( 'column-major', 3, 3, A, 3, TAU, 1, WORK, 1, 3 );
	assert.equal( info, tc.INFO );
	assertArrayClose( A, tc.A, 1e-13, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-13, 'TAU' );
});

test( 'dgeqlf column-major 4x3 matches fixture', function t() {
	var tc = findCase( '4x3' );
	var A = new Float64Array( [ 2, 1, 3, 1, 1, 4, 2, 3, 3, 2, 5, 1 ] );
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 );
	var info = dgeqlf( 'column-major', 4, 3, A, 4, TAU, 1, WORK, 1, 3 );
	assert.equal( info, tc.INFO );
	assertArrayClose( A, tc.A, 1e-13, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-13, 'TAU' );
});

test( 'dgeqlf row-major 3x3 matches transposed fixture', function t() {
	// Row-major MxN is physically equivalent to column-major NxM (transpose).
	// Here M=N=3 so we just need to store the logical matrix in row-major order.
	var tc = findCase( '3x3' );
	var M = 3;
	var N = 3;
	var cmSrc = [ 2, 1, 3, 1, 4, 2, 3, 2, 5 ];
	var A = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ ( i * N ) + j ] = cmSrc[ i + ( j * M ) ];
		}
	}
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 );
	var info = dgeqlf( 'row-major', M, N, A, N, TAU, 1, WORK, 1, 3 );
	assert.equal( info, tc.INFO );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			assertClose( A[ ( i * N ) + j ], tc.A[ i + ( j * M ) ], 1e-13, 'A[' + i + ',' + j + ']' );
		}
	}
	assertArrayClose( TAU, tc.TAU, 1e-13, 'TAU' );
});

