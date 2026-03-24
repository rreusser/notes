'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgetrf = require( '../../dgetrf/lib/base.js' );
var dgetrs = require( '../../dgetrs/lib/base.js' );
var dgerfs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgerfs.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr;
	if ( expected === 0.0 ) {
		assert.ok( Math.abs( actual ) <= tol, msg + ': expected ~0, got ' + actual );
		return;
	}
	relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Sets up a system: factorizes A, solves for initial X, returns all arrays.
*
* @private
* @param {string} trans - 'no-transpose' or 'transpose'
* @param {Array} aVals - column-major values for A (N*N)
* @param {Array} bVals - column-major values for B (N*NRHS)
* @param {number} N - matrix order
* @param {number} nrhs - number of right-hand sides
* @returns {Object} { A, AF, IPIV, B, X, FERR, BERR }
*/
function setupSystem( trans, aVals, bVals, N, nrhs ) {
	var IPIV = new Int32Array( N );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var A = new Float64Array( aVals );
	var AF = new Float64Array( aVals );
	var B = new Float64Array( bVals );
	var X = new Float64Array( bVals );
	var info;

	// Factorize AF = P*L*U
	info = dgetrf( N, N, AF, 1, N, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'dgetrf should succeed' );

	// Initial solve: op(AF) * X = B
	info = dgetrs( trans, N, nrhs, AF, 1, N, 0, IPIV, 1, 0, X, 1, N, 0 );
	assert.equal( info, 0, 'dgetrs should succeed' );

	return {
		A: A,
		AF: AF,
		IPIV: IPIV,
		B: B,
		X: X,
		FERR: FERR,
		BERR: BERR
	};
}


// TESTS //

test( 'dgerfs: basic_3x3', function t() {
	var tc = findCase( 'basic_3x3' );
	var sys = setupSystem( 'no-transpose', [
		2, 4, 8,
		1, 3, 7,
		1, 3, 9
	], [ 1, 1, 1 ], 3, 1 );
	var info;

	info = dgerfs( 'no-transpose', 3, 1,
		sys.A, 1, 3, 0,
		sys.AF, 1, 3, 0,
		sys.IPIV, 1, 0,
		sys.B, 1, 3, 0,
		sys.X, 1, 3, 0,
		sys.FERR, 1, 0,
		sys.BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( sys.X.subarray( 0, 3 ) ), tc.x, 1e-12, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR >= 0' );
	assert.ok( sys.BERR[ 0 ] < 1e-10, 'BERR is small' );
});

test( 'dgerfs: transpose_3x3', function t() {
	var tc = findCase( 'transpose_3x3' );
	var sys = setupSystem( 'transpose', [
		2, 4, 8,
		1, 3, 7,
		1, 3, 9
	], [ 1, 1, 1 ], 3, 1 );
	var info;

	info = dgerfs( 'transpose', 3, 1,
		sys.A, 1, 3, 0,
		sys.AF, 1, 3, 0,
		sys.IPIV, 1, 0,
		sys.B, 1, 3, 0,
		sys.X, 1, 3, 0,
		sys.FERR, 1, 0,
		sys.BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( sys.X.subarray( 0, 3 ) ), tc.x, 1e-12, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR >= 0' );
	assert.ok( sys.BERR[ 0 ] < 1e-10, 'BERR is small' );
});

test( 'dgerfs: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var sys = setupSystem( 'no-transpose', [
		2, 4, 8,
		1, 3, 7,
		1, 3, 9
	], [ 1, 0, 0, 0, 1, 0 ], 3, 2 );
	var info;

	info = dgerfs( 'no-transpose', 3, 2,
		sys.A, 1, 3, 0,
		sys.AF, 1, 3, 0,
		sys.IPIV, 1, 0,
		sys.B, 1, 3, 0,
		sys.X, 1, 3, 0,
		sys.FERR, 1, 0,
		sys.BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( sys.X.subarray( 0, 6 ) ), tc.x, 1e-12, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR[0] >= 0' );
	assert.ok( sys.FERR[ 1 ] >= 0.0, 'FERR[1] >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR[0] >= 0' );
	assert.ok( sys.BERR[ 1 ] >= 0.0, 'BERR[1] >= 0' );
});

test( 'dgerfs: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var AF = new Float64Array( 1 );
	var IPIV = new Int32Array( 1 );
	var B = new Float64Array( 1 );
	var X = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var info;

	info = dgerfs( 'no-transpose', 0, 1,
		A, 1, 1, 0,
		AF, 1, 1, 0,
		IPIV, 1, 0,
		B, 1, 1, 0,
		X, 1, 1, 0,
		FERR, 1, 0,
		BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assert.equal( FERR[ 0 ], 0.0, 'FERR = 0 for N=0' );
	assert.equal( BERR[ 0 ], 0.0, 'BERR = 0 for N=0' );
});

test( 'dgerfs: nrhs_zero', function t() {
	var tc = findCase( 'nrhs_zero' );
	var A = new Float64Array( 9 );
	var AF = new Float64Array( 9 );
	var IPIV = new Int32Array( 3 );
	var B = new Float64Array( 3 );
	var X = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var info;

	info = dgerfs( 'no-transpose', 3, 0,
		A, 1, 3, 0,
		AF, 1, 3, 0,
		IPIV, 1, 0,
		B, 1, 3, 0,
		X, 1, 3, 0,
		FERR, 1, 0,
		BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
});

test( 'dgerfs: hilbert_3x3', function t() {
	var tc = findCase( 'hilbert_3x3' );
	var sys = setupSystem( 'no-transpose', [
		1.0, 0.5, 1.0 / 3.0,
		0.5, 1.0 / 3.0, 0.25,
		1.0 / 3.0, 0.25, 0.2
	], [ 1, 1, 1 ], 3, 1 );
	var info;

	info = dgerfs( 'no-transpose', 3, 1,
		sys.A, 1, 3, 0,
		sys.AF, 1, 3, 0,
		sys.IPIV, 1, 0,
		sys.B, 1, 3, 0,
		sys.X, 1, 3, 0,
		sys.FERR, 1, 0,
		sys.BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( sys.X.subarray( 0, 3 ) ), tc.x, 1e-10, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR >= 0' );
});

test( 'dgerfs: one_by_one', function t() {
	var tc = findCase( 'one_by_one' );
	var sys = setupSystem( 'no-transpose', [ 5.0 ], [ 10.0 ], 1, 1 );
	var info;

	info = dgerfs( 'no-transpose', 1, 1,
		sys.A, 1, 1, 0,
		sys.AF, 1, 1, 0,
		sys.IPIV, 1, 0,
		sys.B, 1, 1, 0,
		sys.X, 1, 1, 0,
		sys.FERR, 1, 0,
		sys.BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( sys.X.subarray( 0, 1 ) ), tc.x, 1e-14, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR >= 0' );
});
