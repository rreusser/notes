'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgetc2 = require( '../../dgetc2/lib/base.js' );
var dgesc2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgesc2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgesc2: basic_2x2', function t() {
	var tc = findCase( 'basic_2x2' );
	var N = 2;
	var LDA = 4;
	var A = new Float64Array( LDA * N );
	var IPIV = new Int32Array( N );
	var JPIV = new Int32Array( N );
	var RHS = new Float64Array( N );
	var scale = new Float64Array( 1 );

	// Set up A = [4 3; 2 1] column-major in LDA=4 storage
	A[ 0*LDA + 0 ] = 4.0; A[ 1*LDA + 0 ] = 3.0;
	A[ 0*LDA + 1 ] = 2.0; A[ 1*LDA + 1 ] = 1.0;

	dgetc2( N, A, 1, LDA, 0, IPIV, 1, 0, JPIV, 1, 0 );

	RHS[ 0 ] = 10.0; RHS[ 1 ] = 4.0;
	dgesc2( N, A, 1, LDA, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );

	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( RHS ), tc.rhs, 1e-14, 'rhs' );
});

test( 'dgesc2: basic_3x3', function t() {
	var tc = findCase( 'basic_3x3' );
	var N = 3;
	var LDA = 4;
	var A = new Float64Array( LDA * N );
	var IPIV = new Int32Array( N );
	var JPIV = new Int32Array( N );
	var RHS = new Float64Array( N );
	var scale = new Float64Array( 1 );

	A[ 0*LDA + 0 ] = 2.0; A[ 1*LDA + 0 ] = 1.0; A[ 2*LDA + 0 ] = 1.0;
	A[ 0*LDA + 1 ] = 4.0; A[ 1*LDA + 1 ] = 3.0; A[ 2*LDA + 1 ] = 3.0;
	A[ 0*LDA + 2 ] = 8.0; A[ 1*LDA + 2 ] = 7.0; A[ 2*LDA + 2 ] = 9.0;

	dgetc2( N, A, 1, LDA, 0, IPIV, 1, 0, JPIV, 1, 0 );

	RHS[ 0 ] = 4.0; RHS[ 1 ] = 10.0; RHS[ 2 ] = 24.0;
	dgesc2( N, A, 1, LDA, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );

	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( RHS ), tc.rhs, 1e-14, 'rhs' );
});

test( 'dgesc2: basic_4x4', function t() {
	var tc = findCase( 'basic_4x4' );
	var N = 4;
	var LDA = 4;
	var A = new Float64Array( LDA * N );
	var IPIV = new Int32Array( N );
	var JPIV = new Int32Array( N );
	var RHS = new Float64Array( N );
	var scale = new Float64Array( 1 );

	A[ 0*LDA + 0 ] = 5.0; A[ 1*LDA + 0 ] = 7.0; A[ 2*LDA + 0 ] = 6.0; A[ 3*LDA + 0 ] = 5.0;
	A[ 0*LDA + 1 ] = 7.0; A[ 1*LDA + 1 ] = 10.0; A[ 2*LDA + 1 ] = 8.0; A[ 3*LDA + 1 ] = 7.0;
	A[ 0*LDA + 2 ] = 6.0; A[ 1*LDA + 2 ] = 8.0; A[ 2*LDA + 2 ] = 10.0; A[ 3*LDA + 2 ] = 9.0;
	A[ 0*LDA + 3 ] = 5.0; A[ 1*LDA + 3 ] = 7.0; A[ 2*LDA + 3 ] = 9.0; A[ 3*LDA + 3 ] = 10.0;

	dgetc2( N, A, 1, LDA, 0, IPIV, 1, 0, JPIV, 1, 0 );

	RHS[ 0 ] = 23.0; RHS[ 1 ] = 32.0; RHS[ 2 ] = 33.0; RHS[ 3 ] = 31.0;
	dgesc2( N, A, 1, LDA, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );

	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( RHS ), tc.rhs, 1e-14, 'rhs' );
});

test( 'dgesc2: n_equals_1', function t() {
	var tc = findCase( 'n_equals_1' );
	var N = 1;
	var LDA = 4;
	var A = new Float64Array( LDA * 1 );
	var IPIV = new Int32Array( 1 );
	var JPIV = new Int32Array( 1 );
	var RHS = new Float64Array( 1 );
	var scale = new Float64Array( 1 );

	A[ 0 ] = 3.0;

	dgetc2( N, A, 1, LDA, 0, IPIV, 1, 0, JPIV, 1, 0 );

	RHS[ 0 ] = 9.0;
	dgesc2( N, A, 1, LDA, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );

	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( RHS ), tc.rhs, 1e-14, 'rhs' );
});
