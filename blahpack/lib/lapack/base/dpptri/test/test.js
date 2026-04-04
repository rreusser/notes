/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpptrf = require( '../../dpptrf/lib/base.js' );
var dpptri = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dpptri.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	return fixture.find( function find( t ) {
		return t.name === name;
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

test( 'dpptri is a function', function t() {
	assert.equal( typeof dpptri, 'function' );
});

test( 'dpptri: n_zero (N=0, quick return)', function t() {
	var info;
	var ap;

	ap = new Float64Array( [ 99.0 ] );
	info = dpptri( 'upper', 0, ap, 1, 0 );
	assert.equal( info, 0 );
	assert.equal( ap[ 0 ], 99.0 );
});

test( 'dpptri: n_one_upper (N=1, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'n_one_upper' );
	ap = new Float64Array( [ 4.0 ] );
	dpptrf( 'upper', 1, ap, 1, 0 );
	info = dpptri( 'upper', 1, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: n_one_lower (N=1, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'n_one_lower' );
	ap = new Float64Array( [ 9.0 ] );
	dpptrf( 'lower', 1, ap, 1, 0 );
	info = dpptri( 'lower', 1, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: upper_2x2 (N=2, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'upper_2x2' );
	ap = new Float64Array( [ 4.0, 2.0, 5.0 ] );
	dpptrf( 'upper', 2, ap, 1, 0 );
	info = dpptri( 'upper', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: lower_2x2 (N=2, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'lower_2x2' );
	ap = new Float64Array( [ 4.0, 2.0, 5.0 ] );
	dpptrf( 'lower', 2, ap, 1, 0 );
	info = dpptri( 'lower', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: identity_upper (N=3, identity matrix, uplo=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ap;

	tc = findCase( 'identity_upper' );
	ap = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
	dpptrf( 'upper', 3, ap, 1, 0 );
	info = dpptri( 'upper', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: identity_lower (N=3, identity matrix, uplo=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ap;

	tc = findCase( 'identity_lower' );
	ap = new Float64Array( [ 1.0, 0.0, 0.0, 1.0, 0.0, 1.0 ] );
	dpptrf( 'lower', 3, ap, 1, 0 );
	info = dpptri( 'lower', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: upper_3x3 (N=3, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'upper_3x3' );
	ap = new Float64Array( [ 25.0, 5.0, 10.0, -5.0, 2.0, 6.0 ] );
	dpptrf( 'upper', 3, ap, 1, 0 );
	info = dpptri( 'upper', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: lower_3x3 (N=3, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'lower_3x3' );
	ap = new Float64Array( [ 25.0, 5.0, -5.0, 10.0, 2.0, 6.0 ] );
	dpptrf( 'lower', 3, ap, 1, 0 );
	info = dpptri( 'lower', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: upper_4x4 (N=4, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'upper_4x4' );
	ap = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 1.0, 5.0, 0.5, 0.5, 1.0, 5.0 ] );
	dpptrf( 'upper', 4, ap, 1, 0 );
	info = dpptri( 'upper', 4, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: lower_4x4 (N=4, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'lower_4x4' );
	ap = new Float64Array( [ 4.0, 2.0, 1.0, 0.5, 5.0, 1.0, 0.5, 5.0, 1.0, 5.0 ] );
	dpptrf( 'lower', 4, ap, 1, 0 );
	info = dpptri( 'lower', 4, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dpptri: singular_factor_upper (info > 0)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'singular_factor_upper' );
	ap = new Float64Array( [ 1.0, 0.0, 0.0 ] );
	info = dpptri( 'upper', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpptri: singular_factor_lower (info > 0)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'singular_factor_lower' );
	ap = new Float64Array( [ 0.0, 1.0, 2.0 ] );
	info = dpptri( 'lower', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpptri: upper_5x5 (N=5, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'upper_5x5' );
	ap = new Float64Array( [ 11.0, 1.0, 11.0, 1.0, 1.0, 11.0, 1.0, 1.0, 1.0, 11.0, 1.0, 1.0, 1.0, 1.0, 11.0 ] ); // eslint-disable-line max-len
	dpptrf( 'upper', 5, ap, 1, 0 );
	info = dpptri( 'upper', 5, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-12, 'ap' );
});

test( 'dpptri: lower_5x5 (N=5, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = findCase( 'lower_5x5' );
	ap = new Float64Array( [ 11.0, 1.0, 1.0, 1.0, 1.0, 11.0, 1.0, 1.0, 1.0, 11.0, 1.0, 1.0, 11.0, 1.0, 11.0 ] ); // eslint-disable-line max-len
	dpptrf( 'lower', 5, ap, 1, 0 );
	info = dpptri( 'lower', 5, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-12, 'ap' );
});

test( 'dpptri: supports non-unit stride (upper)', function t() {
	var info;
	var ap;

	ap = new Float64Array( [ 4.0, -1.0, 2.0, -1.0, 5.0 ] );
	dpptrf( 'upper', 2, ap, 2, 0 );
	info = dpptri( 'upper', 2, ap, 2, 0 );
	assert.equal( info, 0 );
	assert.ok( Math.abs( ap[ 0 ] - 0.3125 ) < 1e-14 );
	assert.ok( Math.abs( ap[ 2 ] - ( -0.125 ) ) < 1e-14 );
	assert.ok( Math.abs( ap[ 4 ] - 0.25 ) < 1e-14 );
	assert.equal( ap[ 1 ], -1.0 );
	assert.equal( ap[ 3 ], -1.0 );
});

test( 'dpptri: supports offset (upper)', function t() {
	var info;
	var ap;

	ap = new Float64Array( [ 0.0, 0.0, 0.0, 4.0 ] );
	dpptrf( 'upper', 1, ap, 1, 3 );
	info = dpptri( 'upper', 1, ap, 1, 3 );
	assert.equal( info, 0 );
	assert.ok( Math.abs( ap[ 3 ] - 0.25 ) < 1e-14 );
	assert.equal( ap[ 0 ], 0.0 );
});

test( 'dpptri: supports non-unit stride (lower)', function t() {
	var info;
	var ap;

	ap = new Float64Array( [ 4.0, -1.0, 2.0, -1.0, 5.0 ] );
	dpptrf( 'lower', 2, ap, 2, 0 );
	info = dpptri( 'lower', 2, ap, 2, 0 );
	assert.equal( info, 0 );
	assert.ok( Math.abs( ap[ 0 ] - 0.3125 ) < 1e-14 );
	assert.ok( Math.abs( ap[ 2 ] - ( -0.125 ) ) < 1e-14 );
	assert.ok( Math.abs( ap[ 4 ] - 0.25 ) < 1e-14 );
	assert.equal( ap[ 1 ], -1.0 );
	assert.equal( ap[ 3 ], -1.0 );
});

test( 'dpptri: supports offset (lower)', function t() {
	var info;
	var ap;

	ap = new Float64Array( [ 0.0, 0.0, 9.0 ] );
	dpptrf( 'lower', 1, ap, 1, 2 );
	info = dpptri( 'lower', 1, ap, 1, 2 );
	assert.equal( info, 0 );
	assert.ok( Math.abs( ap[ 2 ] - ( 1.0 / 9.0 ) ) < 1e-14 );
	assert.equal( ap[ 0 ], 0.0 );
});
