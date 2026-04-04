'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dppcon = require( './../lib/base.js' );
var dpptrf = require( '../../dpptrf/lib/base.js' );
var dlansp = require( '../../dlansp/lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dppcon.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

/**
* Helper to compute condition number of packed SPD matrix.
*
* @private
* @param {string} uploStr - 'upper' or 'lower'
* @param {NonNegativeInteger} N - order
* @param {Array} apFlat - packed array values
* @returns {Object} { rcond, anorm, info }
*/
function computeRcond( uploStr, N, apFlat ) {
	var AP = new Float64Array( apFlat );
	var work = new Float64Array( 3 * N );
	var iwork = new Int32Array( N );
	var rcond = new Float64Array( 1 );
	var anorm;
	var info;

	anorm = dlansp( 'one-norm', uploStr, N, AP, 1, 0, work, 1, 0 );
	dpptrf( uploStr, N, AP, 1, 0 );
	info = dppcon( uploStr, N, AP, 1, 0, anorm, rcond, work, 1, 0, iwork, 1, 0 );
	return { rcond: rcond[ 0 ], anorm: anorm, info: info };
}


// TESTS //

test( 'dppcon: main export is a function', function t() {
	assert.strictEqual( typeof dppcon, 'function' );
});

test( 'dppcon: 3x3 identity, upper packed (rcond = 1)', function t() {
	var result;
	var tc;

	tc = findCase( 'identity_upper' );
	// Upper packed: A(1,1)=1, A(1,2)=0, A(2,2)=1, A(1,3)=0, A(2,3)=0, A(3,3)=1
	result = computeRcond( 'upper', 3, [ 1, 0, 1, 0, 0, 1 ] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dppcon: 3x3 identity, lower packed (rcond = 1)', function t() {
	var result;
	var tc;

	tc = findCase( 'identity_lower' );
	// Lower packed: A(1,1)=1, A(2,1)=0, A(3,1)=0, A(2,2)=1, A(3,2)=0, A(3,3)=1
	result = computeRcond( 'lower', 3, [ 1, 0, 0, 1, 0, 1 ] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dppcon: well-conditioned SPD 3x3, upper packed', function t() {
	var result;
	var tc;

	tc = findCase( 'well_cond_upper' );
	// A = [[4, 1, 1], [1, 3, 1], [1, 1, 2]]
	// Upper packed: 4, 1, 3, 1, 1, 2
	result = computeRcond( 'upper', 3, [ 4, 1, 3, 1, 1, 2 ] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dppcon: well-conditioned SPD 3x3, lower packed', function t() {
	var result;
	var tc;

	tc = findCase( 'well_cond_lower' );
	// A = [[4, 1, 1], [1, 3, 1], [1, 1, 2]]
	// Lower packed: 4, 1, 1, 3, 1, 2
	result = computeRcond( 'lower', 3, [ 4, 1, 1, 3, 1, 2 ] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dppcon: N=0 (rcond = 1)', function t() {
	var rcond;
	var work;
	var iwork;
	var info;
	var AP;

	AP = new Float64Array( 0 );
	work = new Float64Array( 0 );
	iwork = new Int32Array( 0 );
	rcond = new Float64Array( 1 );
	info = dppcon( 'upper', 0, AP, 1, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 1.0 );
});

test( 'dppcon: anorm = 0 (rcond = 0)', function t() {
	var rcond;
	var work;
	var iwork;
	var info;
	var AP;

	// 3x3 identity, upper packed, already factored
	AP = new Float64Array( [ 1, 0, 1, 0, 0, 1 ] );
	work = new Float64Array( 9 );
	iwork = new Int32Array( 3 );
	rcond = new Float64Array( 1 );
	info = dppcon( 'upper', 3, AP, 1, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 0.0 );
});

test( 'dppcon: N=1 (rcond = 1 for scalar)', function t() {
	var result;
	var tc;

	tc = findCase( 'n_one' );
	// 1x1 matrix: AP = [5]
	result = computeRcond( 'upper', 1, [ 5 ] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dppcon: 4x4 SPD, upper packed', function t() {
	var result;
	var tc;

	tc = findCase( '4x4_upper' );
	// A = [[10, 1, 2, 0], [1, 8, 1, 1], [2, 1, 6, 1], [0, 1, 1, 5]]
	// Upper packed: 10, 1, 8, 2, 1, 6, 0, 1, 1, 5
	result = computeRcond( 'upper', 4, [ 10, 1, 8, 2, 1, 6, 0, 1, 1, 5 ] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dppcon: 4x4 SPD, lower packed', function t() {
	var result;
	var tc;

	tc = findCase( '4x4_lower' );
	// Same matrix, lower packed: 10, 1, 2, 0, 8, 1, 1, 6, 1, 5
	result = computeRcond( 'lower', 4, [ 10, 1, 2, 0, 8, 1, 1, 6, 1, 5 ] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dppcon: ill-conditioned (rcond near 0)', function t() {
	var result;
	var tc;

	tc = findCase( 'ill_cond' );
	// A = diag(1, 1, 1e-15), upper packed: 1, 0, 1, 0, 0, 1e-15
	result = computeRcond( 'upper', 3, [ 1, 0, 1, 0, 0, 1e-15 ] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-5, 'rcond' );
});

test( 'dppcon: rcond is between 0 and 1 for well-conditioned SPD', function t() {
	var result;

	// 3x3 well-conditioned diagonal-dominant, upper packed
	// A = [[10, 1, 0], [1, 10, 1], [0, 1, 10]]
	// Upper packed: 10, 1, 10, 0, 1, 10
	result = computeRcond( 'upper', 3, [ 10, 1, 10, 0, 1, 10 ] );
	assert.strictEqual( result.info, 0 );
	assert.ok( result.rcond > 0, 'rcond > 0' );
	assert.ok( result.rcond <= 1, 'rcond <= 1' );
});

test( 'dppcon: 2x2 SPD, lower packed', function t() {
	var result;

	// A = [[4, 1], [1, 3]]
	// Lower packed: 4, 1, 3
	result = computeRcond( 'lower', 2, [ 4, 1, 3 ] );
	assert.strictEqual( result.info, 0 );
	assert.ok( result.rcond > 0, 'rcond > 0' );
	assert.ok( result.rcond <= 1, 'rcond <= 1' );
});
