/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpptrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dpptrf.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1e-14 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}


// TESTS //

test( 'dpptrf is a function', function t() {
	assert.equal( typeof dpptrf, 'function' );
});

test( 'dpptrf: upper_basic (N=4, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'upper_basic' );
	ap = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 1.0, 5.0, 0.5, 0.5, 1.0, 5.0 ] );
	info = dpptrf( 'upper', 4, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: lower_basic (N=4, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'lower_basic' );
	ap = new Float64Array( [ 4.0, 2.0, 1.0, 0.5, 5.0, 1.0, 0.5, 5.0, 1.0, 5.0 ] );
	info = dpptrf( 'lower', 4, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: n_zero (N=0, quick return)', function t() {
	var info;
	var ap;

	ap = new Float64Array( [ 99.0 ] );
	info = dpptrf( 'upper', 0, ap, 1, 0 );
	assert.equal( info, 0 );
	assert.equal( ap[ 0 ], 99.0 );
});

test( 'dpptrf: n_one_upper (N=1, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'n_one_upper' );
	ap = new Float64Array( [ 9.0 ] );
	info = dpptrf( 'upper', 1, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: n_one_lower (N=1, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'n_one_lower' );
	ap = new Float64Array( [ 16.0 ] );
	info = dpptrf( 'lower', 1, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: not_spd_upper (info > 0, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'not_spd_upper' );
	ap = new Float64Array( [ 1.0, 2.0, 1.0 ] );
	info = dpptrf( 'upper', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: not_spd_lower (info > 0, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'not_spd_lower' );
	ap = new Float64Array( [ 1.0, 2.0, 1.0 ] );
	info = dpptrf( 'lower', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: identity_upper (N=3, identity matrix, uplo=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ap;

	tc = findCase( 'identity_upper' );
	ap = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
	info = dpptrf( 'upper', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: identity_lower (N=3, identity matrix, uplo=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ap;

	tc = findCase( 'identity_lower' );
	ap = new Float64Array( [ 1.0, 0.0, 0.0, 1.0, 0.0, 1.0 ] );
	info = dpptrf( 'lower', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: upper_3x3 (N=3, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'upper_3x3' );
	ap = new Float64Array( [ 25.0, 5.0, 10.0, -5.0, 2.0, 6.0 ] );
	info = dpptrf( 'upper', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: lower_3x3 (N=3, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'lower_3x3' );
	ap = new Float64Array( [ 25.0, 5.0, -5.0, 10.0, 2.0, 6.0 ] );
	info = dpptrf( 'lower', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: not_spd_first_upper (info=1 at first diagonal, uplo=upper)', function t() { // eslint-disable-line max-len
	var info;
	var ap;

	ap = new Float64Array( [ -4.0, 1.0, 5.0 ] );
	info = dpptrf( 'upper', 2, ap, 1, 0 );
	assert.equal( info, 1 );
});

test( 'dpptrf: not_spd_first_lower (info=1 at first diagonal, uplo=lower)', function t() { // eslint-disable-line max-len
	var info;
	var ap;

	ap = new Float64Array( [ 0.0, 1.0, 5.0 ] );
	info = dpptrf( 'lower', 2, ap, 1, 0 );
	assert.equal( info, 1 );
});

test( 'dpptrf: upper_2x2 (N=2, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'upper_2x2' );
	ap = new Float64Array( [ 4.0, 2.0, 5.0 ] );
	info = dpptrf( 'upper', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: lower_2x2 (N=2, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'lower_2x2' );
	ap = new Float64Array( [ 4.0, 2.0, 5.0 ] );
	info = dpptrf( 'lower', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptrf: supports non-unit stride (upper)', function t() {
	var info;
	var ap;

	ap = new Float64Array( [ 4.0, -1.0, 2.0, -1.0, 5.0 ] );
	info = dpptrf( 'upper', 2, ap, 2, 0 );
	assert.equal( info, 0 );
	assert.equal( ap[ 0 ], 2.0 );
	assert.equal( ap[ 2 ], 1.0 );
	assert.equal( ap[ 4 ], 2.0 );
	assert.equal( ap[ 1 ], -1.0 );
	assert.equal( ap[ 3 ], -1.0 );
});

test( 'dpptrf: supports offset (lower)', function t() {
	var info;
	var ap;

	ap = new Float64Array( [ 0.0, 0.0, 0.0, 25.0 ] );
	info = dpptrf( 'lower', 1, ap, 1, 3 );
	assert.equal( info, 0 );
	assert.equal( ap[ 3 ], 5.0 );
	assert.equal( ap[ 0 ], 0.0 );
});

test( 'dpptrf: supports non-unit stride (lower)', function t() {
	var info;
	var ap;

	ap = new Float64Array( [ 4.0, -1.0, 2.0, -1.0, 5.0 ] );
	info = dpptrf( 'lower', 2, ap, 2, 0 );
	assert.equal( info, 0 );
	assert.equal( ap[ 0 ], 2.0 );
	assert.equal( ap[ 2 ], 1.0 );
	assert.equal( ap[ 4 ], 2.0 );
	assert.equal( ap[ 1 ], -1.0 );
	assert.equal( ap[ 3 ], -1.0 );
});
