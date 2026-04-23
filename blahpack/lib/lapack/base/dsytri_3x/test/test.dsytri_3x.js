/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var path = require( 'path' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsytri3x = require( './../lib/dsytri_3x.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dsytri_3x.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync, max-len
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


// FUNCTIONS //

/**
* Finds a fixture by name.
*
* @private
* @param {string} name - test case name
* @returns {Object} fixture entry
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	});
}


// TESTS //

test( 'dsytri_3x is a function', function t() {
	assert.strictEqual( typeof dsytri3x, 'function', 'is a function' );
});

test( 'dsytri_3x has expected arity', function t() {
	assert.strictEqual( dsytri3x.length, 13, 'has expected arity' );
});

test( 'dsytri_3x: column-major layout matches ndarray path', function t() {
	var ldwork;
	var ipiv;
	var work;
	var info;
	var tc;
	var A;
	var e;
	var i;
	tc = findCase( '4x4_lower_def_nb2' );
	A = new Float64Array( tc.a_factored );
	e = new Float64Array( tc.e );
	ipiv = new Int32Array( tc.ipiv.length );
	for ( i = 0; i < tc.ipiv.length; i++ ) {
		ipiv[ i ] = ( tc.ipiv[ i ] > 0 ) ? ( tc.ipiv[ i ] - 1 ) : tc.ipiv[ i ];
	}
	ldwork = 4 + 2 + 1;
	work = new Float64Array( ldwork * 5 );
	info = dsytri3x( 'column-major', 'lower', 4, A, 4, e, 1, ipiv, 1, 0, work, 1, 2 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.ok( Math.abs( A[ 0 ] - tc.a_inv[ 0 ] ) < 1e-11, 'A[0,0]' );
});

test( 'dsytri_3x throws TypeError for invalid order', function t() {
	var ipiv = new Int32Array( 2 );
	var work = new Float64Array( 10 );
	var A = new Float64Array( 4 );
	var e = new Float64Array( 2 );
	assert.throws( function fn() {
		dsytri3x( 'invalid', 'upper', 2, A, 2, e, 1, ipiv, 1, 0, work, 1, 1 );
	}, TypeError );
});

test( 'dsytri_3x throws TypeError for invalid uplo', function t() {
	var ipiv = new Int32Array( 2 );
	var work = new Float64Array( 10 );
	var A = new Float64Array( 4 );
	var e = new Float64Array( 2 );
	assert.throws( function fn() {
		dsytri3x( 'column-major', 'invalid', 2, A, 2, e, 1, ipiv, 1, 0, work, 1, 1 );
	}, TypeError );
});

test( 'dsytri_3x throws RangeError for negative N', function t() {
	var ipiv = new Int32Array( 2 );
	var work = new Float64Array( 10 );
	var A = new Float64Array( 4 );
	var e = new Float64Array( 2 );
	assert.throws( function fn() {
		dsytri3x( 'column-major', 'upper', -1, A, 2, e, 1, ipiv, 1, 0, work, 1, 1 );
	}, RangeError );
});

test( 'dsytri_3x throws RangeError for invalid LDA', function t() {
	var ipiv = new Int32Array( 2 );
	var work = new Float64Array( 10 );
	var A = new Float64Array( 4 );
	var e = new Float64Array( 2 );
	assert.throws( function fn() {
		dsytri3x( 'row-major', 'upper', 4, A, 1, e, 1, ipiv, 1, 0, work, 1, 1 );
	}, RangeError );
});
