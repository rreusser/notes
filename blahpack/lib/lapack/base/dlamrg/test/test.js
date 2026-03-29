/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlamrg = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlamrg.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}


// TESTS //

test( 'dlamrg: both forward, 3+3', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = findCase( 'fwd_fwd_3_3' );
	a = new Float64Array( [ 1.0, 3.0, 5.0, 2.0, 4.0, 6.0 ] );
	idx = new Int32Array( 6 );
	dlamrg( 3, 3, a, 1, 0, 1, 1, idx, 1, 0 );
	for ( i = 0; i < 6; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: first forward, second backward, 3+3', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = findCase( 'fwd_bwd_3_3' );
	a = new Float64Array( [ 1.0, 3.0, 5.0, 6.0, 4.0, 2.0 ] );
	idx = new Int32Array( 6 );
	dlamrg( 3, 3, a, 1, 0, 1, -1, idx, 1, 0 );
	for ( i = 0; i < 6; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: first backward, second forward, 3+2', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = findCase( 'bwd_fwd_3_2' );
	a = new Float64Array( [ 5.0, 3.0, 1.0, 2.0, 4.0 ] );
	idx = new Int32Array( 5 );
	dlamrg( 3, 2, a, 1, 0, -1, 1, idx, 1, 0 );
	for ( i = 0; i < 5; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: both backward, 2+3', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = findCase( 'bwd_bwd_2_3' );
	a = new Float64Array( [ 4.0, 2.0, 6.0, 3.0, 1.0 ] );
	idx = new Int32Array( 5 );
	dlamrg( 2, 3, a, 1, 0, -1, -1, idx, 1, 0 );
	for ( i = 0; i < 5; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: n1=1, n1=1', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = findCase( 'n1_n1' );
	a = new Float64Array( [ 3.0, 1.0 ] );
	idx = new Int32Array( 2 );
	dlamrg( 1, 1, a, 1, 0, 1, 1, idx, 1, 0 );
	for ( i = 0; i < 2; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: equal elements', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = findCase( 'equal' );
	a = new Float64Array( [ 2.0, 2.0, 2.0, 2.0 ] );
	idx = new Int32Array( 4 );
	dlamrg( 2, 2, a, 1, 0, 1, 1, idx, 1, 0 );
	for ( i = 0; i < 4; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: n1=4, n2=1', function t() {
	var idx;
	var tc;
	var a;
	var i;

	tc = findCase( 'n4_n1' );
	a = new Float64Array( [ 1.0, 3.0, 5.0, 7.0, 4.0 ] );
	idx = new Int32Array( 5 );
	dlamrg( 4, 1, a, 1, 0, 1, 1, idx, 1, 0 );
	for ( i = 0; i < 5; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});
