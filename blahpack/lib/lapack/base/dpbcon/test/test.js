/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dpbtrf = require( './../../dpbtrf/lib/base.js' );
var dpbcon = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dpbcon.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* BandedMatrix.
*
* @private
* @param {*} ldab - ldab
* @param {*} n - n
* @param {*} entries - entries
* @returns {*} result
*/
function bandedMatrix( ldab, n, entries ) {
	var ab = new Float64Array( ldab * n );
	var i;
	for ( i = 0; i < entries.length; i++ ) {
		ab[ (entries[i][1] * ldab) + entries[i][0] ] = entries[i][2];
	}
	return ab;
}


// TESTS //

test( 'dpbcon: upper KD=1, N=4', function t() {
	var rcond;
	var iwork;
	var work;
	var info;
	var tc;
	var ab;
	var n;

	tc = findCase( 'upper_kd1' );
	n = 4;
	ab = bandedMatrix( 2, n, [
		[1, 0, 4.0],
		[0, 1, 1.0],
		[1, 1, 5.0],
		[0, 2, 1.0],
		[1, 2, 6.0],
		[0, 3, 2.0],
		[1, 3, 7.0]
	]);
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );
	info = dpbtrf( 'upper', n, 1, ab, 1, 2, 0 );
	assert.equal( info, 0, 'dpbtrf info' );
	info = dpbcon( 'upper', n, 1, ab, 1, 2, 0, tc.anorm, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-10, 'rcond' );
});

test( 'dpbcon: lower KD=1, N=4', function t() {
	var rcond;
	var iwork;
	var work;
	var info;
	var tc;
	var ab;
	var n;

	tc = findCase( 'lower_kd1' );
	n = 4;
	ab = bandedMatrix( 2, n, [
		[0, 0, 4.0],
		[1, 0, 1.0],
		[0, 1, 5.0],
		[1, 1, 1.0],
		[0, 2, 6.0],
		[1, 2, 2.0],
		[0, 3, 7.0]
	]);
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );
	info = dpbtrf( 'lower', n, 1, ab, 1, 2, 0 );
	assert.equal( info, 0, 'dpbtrf info' );
	info = dpbcon( 'lower', n, 1, ab, 1, 2, 0, tc.anorm, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-10, 'rcond' );
});

test( 'dpbcon: upper KD=2, N=4', function t() {
	var rcond;
	var iwork;
	var work;
	var info;
	var tc;
	var ab;
	var n;

	tc = findCase( 'upper_kd2' );
	n = 4;
	ab = bandedMatrix( 3, n, [
		[2, 0, 10.0],
		[1, 1, 2.0],
		[2, 1, 10.0],
		[0, 2, 1.0],
		[1, 2, 3.0],
		[2, 2, 10.0],
		[0, 3, 1.0],
		[1, 3, 2.0],
		[2, 3, 10.0]
	]);
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );
	info = dpbtrf( 'upper', n, 2, ab, 1, 3, 0 );
	assert.equal( info, 0, 'dpbtrf info' );
	info = dpbcon( 'upper', n, 2, ab, 1, 3, 0, tc.anorm, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-10, 'rcond' );
});

test( 'dpbcon: N=0', function t() {
	var rcond;
	var iwork;
	var work;
	var info;
	var tc;
	var ab;

	tc = findCase( 'n_zero' );
	ab = new Float64Array( 1 );
	work = new Float64Array( 3 );
	iwork = new Int32Array( 1 );
	rcond = new Float64Array( 1 );
	info = dpbcon( 'upper', 0, 0, ab, 1, 1, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-14, 'rcond' );
});

test( 'dpbcon: N=1', function t() {
	var rcond;
	var iwork;
	var work;
	var info;
	var tc;
	var ab;

	tc = findCase( 'n_one' );
	ab = new Float64Array([ 4.0 ]);
	work = new Float64Array( 3 );
	iwork = new Int32Array( 1 );
	rcond = new Float64Array( 1 );
	info = dpbtrf( 'upper', 1, 0, ab, 1, 1, 0 );
	assert.equal( info, 0, 'dpbtrf info' );
	info = dpbcon( 'upper', 1, 0, ab, 1, 1, 0, 4.0, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-14, 'rcond' );
});

test( 'dpbcon: anorm=0', function t() {
	var rcond;
	var iwork;
	var work;
	var info;
	var tc;
	var ab;
	var n;

	tc = findCase( 'anorm_zero' );
	n = 4;
	ab = bandedMatrix( 2, n, [
		[1, 0, 4.0],
		[0, 1, 1.0],
		[1, 1, 5.0],
		[0, 2, 1.0],
		[1, 2, 6.0],
		[0, 3, 2.0],
		[1, 3, 7.0]
	]);
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );
	dpbtrf( 'upper', n, 1, ab, 1, 2, 0 );
	info = dpbcon( 'upper', n, 1, ab, 1, 2, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-14, 'rcond' );
});
