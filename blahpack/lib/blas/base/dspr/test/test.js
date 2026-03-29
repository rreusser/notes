/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dspr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dspr.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dspr: upper_basic (uplo=U, N=3, alpha=1, unit stride)', function t() {
	var tc = findCase( 'upper_basic' );
	// AP = upper triangle of identity: diag at positions 0,2,5
	var AP = new Float64Array( [ 1, 0, 1, 0, 0, 1 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );

	dspr( 'upper', 3, 1.0, x, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.ap, 1e-14, 'ap' );
});

test( 'dspr: lower_basic (uplo=L, N=3, alpha=1, unit stride)', function t() {
	var tc = findCase( 'lower_basic' );
	// AP = lower triangle of identity: diag at positions 0,3,5
	var AP = new Float64Array( [ 1, 0, 0, 1, 0, 1 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );

	dspr( 'lower', 3, 1.0, x, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.ap, 1e-14, 'ap' );
});

test( 'dspr: alpha2 (uplo=U, N=3, alpha=2)', function t() {
	var tc = findCase( 'alpha2' );
	var AP = new Float64Array( [ 0, 0, 0, 0, 0, 0 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );

	dspr( 'upper', 3, 2.0, x, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.ap, 1e-14, 'ap' );
});

test( 'dspr: n_zero (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var AP = new Float64Array( [ 99 ] );
	var x = new Float64Array( [ 1 ] );

	dspr( 'upper', 0, 1.0, x, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.ap, 1e-14, 'ap' );
});

test( 'dspr: alpha_zero (alpha=0, quick return)', function t() {
	var tc = findCase( 'alpha_zero' );
	var AP = new Float64Array( [ 5, 0, 0, 0, 0, 0 ] );
	var x = new Float64Array( [ 1, 2, 3 ] );

	dspr( 'upper', 3, 0.0, x, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.ap, 1e-14, 'ap' );
});

test( 'dspr: stride (uplo=U, N=3, incx=2)', function t() {
	var tc = findCase( 'stride' );
	var AP = new Float64Array( [ 0, 0, 0, 0, 0, 0 ] );
	var x = new Float64Array( [ 1, 0, 2, 0, 3, 0 ] );

	dspr( 'upper', 3, 1.0, x, 2, 0, AP, 1, 0 );
	assertArrayClose( AP, tc.ap, 1e-14, 'ap' );
});

test( 'dspr: returns AP', function t() {
	var AP = new Float64Array( [ 1 ] );
	var x = new Float64Array( [ 1 ] );

	var result = dspr( 'upper', 1, 1.0, x, 1, 0, AP, 1, 0 );
	assert.equal( result, AP );
});

test( 'dspr: x element zero skips update for that column', function t() {
	// When x[j] === 0, that column should not be updated
	var AP = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var expected = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var x = new Float64Array( [ 0, 0, 0 ] );

	dspr( 'upper', 3, 1.0, x, 1, 0, AP, 1, 0 );
	assertArrayClose( AP, expected, 1e-14, 'ap unchanged' );
});
