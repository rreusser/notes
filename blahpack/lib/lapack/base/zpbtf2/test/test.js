

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpbtf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zpbtf2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zpbtf2: upper_3x3_kd1 (UPLO=U, N=3, KD=1)', function t() {
	var tc = findCase( 'upper_3x3_kd1' );
	// Band storage (upper, LDAB=2): 2 rows x 3 cols
	// Col 1: AB(1,1)=*, AB(2,1)=4
	// Col 2: AB(1,2)=(1+i), AB(2,2)=5
	// Col 3: AB(1,3)=(2-i), AB(2,3)=6
	var AB = new Complex128Array( [
		0, 0, 4, 0,
		1, 1, 5, 0,
		2, -1, 6, 0
	] );
	var info = zpbtf2( 'upper', 3, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( AB, 0 ) ), tc.AB, 1e-14, 'AB' );
});

test( 'zpbtf2: lower_3x3_kd1 (UPLO=L, N=3, KD=1)', function t() {
	var tc = findCase( 'lower_3x3_kd1' );
	// Band storage (lower, LDAB=2): 2 rows x 3 cols
	// Col 1: AB(1,1)=4, AB(2,1)=(1-i)
	// Col 2: AB(1,2)=5, AB(2,2)=(2+i)
	// Col 3: AB(1,3)=6, AB(2,3)=*
	var AB = new Complex128Array( [
		4, 0, 1, -1,
		5, 0, 2, 1,
		6, 0, 0, 0
	] );
	var info = zpbtf2( 'lower', 3, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( AB, 0 ) ), tc.AB, 1e-14, 'AB' );
});

test( 'zpbtf2: n_zero (N=0 quick return)', function t() {
	var AB = new Complex128Array( 4 );
	var info = zpbtf2( 'upper', 0, 1, AB, 1, 2, 0 );
	assert.equal( info, 0 );
});

test( 'zpbtf2: n_one (N=1)', function t() {
	var tc = findCase( 'n_one' );
	var AB = new Complex128Array( [ 9, 0 ] );
	var info = zpbtf2( 'upper', 1, 0, AB, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( AB, 0 ) ), tc.AB, 1e-14, 'AB' );
});

test( 'zpbtf2: upper_4x4_kd2 (UPLO=U, N=4, KD=2)', function t() {
	var tc = findCase( 'upper_4x4_kd2' );
	// Band storage (upper, LDAB=3): 3 rows x 4 cols
	var AB = new Complex128Array( [
		0, 0, 0, 0, 10, 0,
		0, 0, 1, 1, 8, 0,
		0.5, -1, 2, 1, 6, 0,
		0, 0, 1, -1, 7, 0
	] );
	var info = zpbtf2( 'upper', 4, 2, AB, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( AB, 0 ) ), tc.AB, 1e-14, 'AB' );
});

test( 'zpbtf2: lower_4x4_kd2 (UPLO=L, N=4, KD=2)', function t() {
	var tc = findCase( 'lower_4x4_kd2' );
	// Band storage (lower, LDAB=3): 3 rows x 4 cols
	var AB = new Complex128Array( [
		10, 0, 1, -1, 0.5, 1,
		8, 0, 2, -1, 1, 1,
		6, 0, 1, -1, 0, 0,
		7, 0, 0, 0, 0, 0
	] );
	var info = zpbtf2( 'lower', 4, 2, AB, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( AB, 0 ) ), tc.AB, 1e-14, 'AB' );
});

test( 'zpbtf2: not_hpd (upper, not positive definite)', function t() {
	var tc = findCase( 'not_hpd' );
	var AB = new Complex128Array( [
		0, 0, 1, 0,
		2, 1, 1, 0
	] );
	var info = zpbtf2( 'upper', 2, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
});

test( 'zpbtf2: not_hpd_lower (lower, not positive definite)', function t() {
	var tc = findCase( 'not_hpd_lower' );
	var AB = new Complex128Array( [
		1, 0, 2, -1,
		1, 0, 0, 0
	] );
	var info = zpbtf2( 'lower', 2, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
});
