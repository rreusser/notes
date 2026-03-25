

'use strict';

// MODULES //

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgetc2 = require( './../../dgetc2/lib/base.js' );
var dlatdf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlatdf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, aOffset, aStride, expected, n, tol, msg ) {
	var i;
	for ( i = 0; i < n; i++ ) {
		assertClose( actual[ aOffset + ( i * aStride ) ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dlatdf: IJOB=1, 2x2 system', function t() {
	var tc = findCase( 'ijob1_2x2' );
	var Z = new Float64Array( [ 4.0, 2.0, 3.0, 1.0 ] ); // column-major 2x2
	var IPIV = new Int32Array( 2 );
	var JPIV = new Int32Array( 2 );
	dgetc2( 2, Z, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );
	var RHS = new Float64Array( [ 1.0, 1.0 ] );
	var out = dlatdf( 1, 2, Z, 1, 2, 0, RHS, 1, 0, 0.0, 1.0, IPIV, 1, 0, JPIV, 1, 0 );
	assertArrayClose( RHS, 0, 1, tc.rhs, 2, 1e-12, 'rhs' );
	assertClose( out.rdsum, tc.rdsum, 1e-12, 'rdsum' );
	assertClose( out.rdscal, tc.rdscal, 1e-12, 'rdscal' );
});

test( 'dlatdf: IJOB=2, 2x2 system', function t() {
	var tc = findCase( 'ijob2_2x2' );
	var Z = new Float64Array( [ 4.0, 2.0, 3.0, 1.0 ] );
	var IPIV = new Int32Array( 2 );
	var JPIV = new Int32Array( 2 );
	dgetc2( 2, Z, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );
	var RHS = new Float64Array( [ 1.0, 1.0 ] );
	var out = dlatdf( 2, 2, Z, 1, 2, 0, RHS, 1, 0, 0.0, 1.0, IPIV, 1, 0, JPIV, 1, 0 );
	assertArrayClose( RHS, 0, 1, tc.rhs, 2, 1e-12, 'rhs' );
	assertClose( out.rdsum, tc.rdsum, 1e-12, 'rdsum' );
	assertClose( out.rdscal, tc.rdscal, 1e-12, 'rdscal' );
});

test( 'dlatdf: IJOB=1, 3x3 system', function t() {
	var tc = findCase( 'ijob1_3x3' );
	var Z = new Float64Array( [
		5.0, 7.0, 6.0,
		7.0, 10.0, 8.0,
		6.0, 8.0, 10.0
	] );
	var IPIV = new Int32Array( 3 );
	var JPIV = new Int32Array( 3 );
	dgetc2( 3, Z, 1, 3, 0, IPIV, 1, 0, JPIV, 1, 0 );
	var RHS = new Float64Array( [ 1.0, -1.0, 0.5 ] );
	var out = dlatdf( 1, 3, Z, 1, 3, 0, RHS, 1, 0, 1.0, 1.0, IPIV, 1, 0, JPIV, 1, 0 );
	assertArrayClose( RHS, 0, 1, tc.rhs, 3, 1e-12, 'rhs' );
	assertClose( out.rdsum, tc.rdsum, 1e-12, 'rdsum' );
	assertClose( out.rdscal, tc.rdscal, 1e-12, 'rdscal' );
});

test( 'dlatdf: IJOB=2, 3x3 system', function t() {
	var tc = findCase( 'ijob2_3x3' );
	var Z = new Float64Array( [
		5.0, 7.0, 6.0,
		7.0, 10.0, 8.0,
		6.0, 8.0, 10.0
	] );
	var IPIV = new Int32Array( 3 );
	var JPIV = new Int32Array( 3 );
	dgetc2( 3, Z, 1, 3, 0, IPIV, 1, 0, JPIV, 1, 0 );
	var RHS = new Float64Array( [ 1.0, -1.0, 0.5 ] );
	var out = dlatdf( 2, 3, Z, 1, 3, 0, RHS, 1, 0, 1.0, 1.0, IPIV, 1, 0, JPIV, 1, 0 );
	assertArrayClose( RHS, 0, 1, tc.rhs, 3, 1e-12, 'rhs' );
	assertClose( out.rdsum, tc.rdsum, 1e-12, 'rdsum' );
	assertClose( out.rdscal, tc.rdscal, 1e-12, 'rdscal' );
});

test( 'dlatdf: IJOB=1, 4x4 system', function t() {
	var tc = findCase( 'ijob1_4x4' );
	var Z = new Float64Array( [
		5.0, 7.0, 6.0, 5.0,
		7.0, 10.0, 8.0, 7.0,
		6.0, 8.0, 10.0, 9.0,
		5.0, 7.0, 9.0, 10.0
	] );
	var IPIV = new Int32Array( 4 );
	var JPIV = new Int32Array( 4 );
	dgetc2( 4, Z, 1, 4, 0, IPIV, 1, 0, JPIV, 1, 0 );
	var RHS = new Float64Array( [ 1.0, -1.0, 2.0, -0.5 ] );
	var out = dlatdf( 1, 4, Z, 1, 4, 0, RHS, 1, 0, 0.0, 1.0, IPIV, 1, 0, JPIV, 1, 0 );
	assertArrayClose( RHS, 0, 1, tc.rhs, 4, 1e-12, 'rhs' );
	assertClose( out.rdsum, tc.rdsum, 1e-12, 'rdsum' );
	assertClose( out.rdscal, tc.rdscal, 1e-12, 'rdscal' );
});

test( 'dlatdf: IJOB=2, 4x4 system', function t() {
	var tc = findCase( 'ijob2_4x4' );
	var Z = new Float64Array( [
		5.0, 7.0, 6.0, 5.0,
		7.0, 10.0, 8.0, 7.0,
		6.0, 8.0, 10.0, 9.0,
		5.0, 7.0, 9.0, 10.0
	] );
	var IPIV = new Int32Array( 4 );
	var JPIV = new Int32Array( 4 );
	dgetc2( 4, Z, 1, 4, 0, IPIV, 1, 0, JPIV, 1, 0 );
	var RHS = new Float64Array( [ 1.0, -1.0, 2.0, -0.5 ] );
	var out = dlatdf( 2, 4, Z, 1, 4, 0, RHS, 1, 0, 0.0, 1.0, IPIV, 1, 0, JPIV, 1, 0 );
	assertArrayClose( RHS, 0, 1, tc.rhs, 4, 1e-12, 'rhs' );
	assertClose( out.rdsum, tc.rdsum, 1e-12, 'rdsum' );
	assertClose( out.rdscal, tc.rdscal, 1e-12, 'rdscal' );
});
