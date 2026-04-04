/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dtpcon = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dtpcon.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// VARIABLES //

var upperAP = new Float64Array( [ 10, -1, 8, 2, -2, 12, -1, 1, -3, 6 ] ); // upper 4x4
var lowerAP = new Float64Array( [ 5, -2, 1, -1, 7, -1, 2, 9, -3, 11 ] ); // lower 4x4
var illAP = new Float64Array( [ 1e12, 0, 1, 0, 0, 1e-12 ] ); // ill-conditioned 3x3
var identAP = new Float64Array( [ 1, 0, 1, 0, 0, 1 ] ); // identity 3x3
var singleAP = new Float64Array( [ 3.0 ] ); // single element N=1


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
* Runs dtpcon and returns { rcond, info }.
*
* @private
* @param {string} norm - 'one-norm' or 'inf-norm'
* @param {string} uplo - 'upper' or 'lower'
* @param {string} diag - 'unit' or 'non-unit'
* @param {number} n - matrix order
* @param {Float64Array} AP - packed triangular matrix
* @returns {Object} result with rcond and info
*/
function run( norm, uplo, diag, n, AP ) {
	var RCOND = new Float64Array( 1 );
	var IWORK = new Int32Array( Math.max( n, 1 ) );
	var WORK = new Float64Array( 3 * Math.max( n, 1 ) );
	var info;

	info = dtpcon( norm, uplo, diag, n, AP, 1, 0, RCOND, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	return {
		'rcond': RCOND[ 0 ],
		'info': info
	};
}


// TESTS //

test( 'dtpcon: upper, non-unit, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 4, upperAP );
	var tc = findCase( 'upper_nonunit_onenorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtpcon: upper, non-unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 4, upperAP );
	var tc = findCase( 'upper_nonunit_infnorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtpcon: upper, unit, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'unit', 4, upperAP );
	var tc = findCase( 'upper_unit_onenorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtpcon: upper, unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'unit', 4, upperAP );
	var tc = findCase( 'upper_unit_infnorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtpcon: lower, non-unit, one-norm', function t() {
	var res = run( 'one-norm', 'lower', 'non-unit', 4, lowerAP );
	var tc = findCase( 'lower_nonunit_onenorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtpcon: lower, non-unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'lower', 'non-unit', 4, lowerAP );
	var tc = findCase( 'lower_nonunit_infnorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtpcon: lower, unit, one-norm', function t() {
	var res = run( 'one-norm', 'lower', 'unit', 4, lowerAP );
	var tc = findCase( 'lower_unit_onenorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtpcon: lower, unit, inf-norm', function t() {
	var res = run( 'inf-norm', 'lower', 'unit', 4, lowerAP );
	var tc = findCase( 'lower_unit_infnorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-12, 'rcond' );
});

test( 'dtpcon: edge case N=0', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 0, new Float64Array( 0 ) );
	var tc = findCase( 'edge_n0' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtpcon: edge case N=1', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 1, singleAP );
	var tc = findCase( 'edge_n1' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtpcon: ill-conditioned, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 3, illAP );
	var tc = findCase( 'ill_conditioned_onenorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dtpcon: ill-conditioned, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 3, illAP );
	var tc = findCase( 'ill_conditioned_infnorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dtpcon: identity, one-norm', function t() {
	var res = run( 'one-norm', 'upper', 'non-unit', 3, identAP );
	var tc = findCase( 'identity_onenorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});

test( 'dtpcon: identity, inf-norm', function t() {
	var res = run( 'inf-norm', 'upper', 'non-unit', 3, identAP );
	var tc = findCase( 'identity_infnorm' );
	assert.equal( res.info, tc.info, 'info' );
	assertClose( res.rcond, tc.rcond, 1e-14, 'rcond' );
});
