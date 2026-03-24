'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dpotrf = require( '../../dpotrf/lib/base.js' );
var dpotrs = require( '../../dpotrs/lib/base.js' );
var dporfs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dporfs.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Sets up a system: factorizes A with dpotrf, solves with dpotrs, returns all arrays.
*
* @private
* @param {string} uplo - 'upper' or 'lower'
* @param {Array} aVals - column-major values for A (N*N)
* @param {Array} bVals - column-major values for B (N*NRHS)
* @param {number} N - matrix order
* @param {number} nrhs - number of right-hand sides
* @returns {Object} { A, AF, B, X, FERR, BERR }
*/
function setupSystem( uplo, aVals, bVals, N, nrhs ) {
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var A = new Float64Array( aVals );
	var AF = new Float64Array( aVals );
	var B = new Float64Array( bVals );
	var X = new Float64Array( bVals );
	var info;

	// Factorize AF = U^T*U or L*L^T
	info = dpotrf( uplo, N, AF, 1, N, 0 );
	assert.equal( info, 0, 'dpotrf should succeed' );

	// Initial solve: A * X = B
	info = dpotrs( uplo, N, nrhs, AF, 1, N, 0, X, 1, N, 0 );
	assert.equal( info, 0, 'dpotrs should succeed' );

	return {
		A: A,
		AF: AF,
		B: B,
		X: X,
		FERR: FERR,
		BERR: BERR
	};
}


// TESTS //

test( 'dporfs: basic_upper_3x3', function t() {
	var tc = findCase( 'basic_upper_3x3' );
	var sys = setupSystem( 'upper', [
		4, 2, 1,
		2, 5, 3,
		1, 3, 6
	], [ 1, 1, 1 ], 3, 1 );
	var info;

	info = dporfs( 'upper', 3, 1,
		sys.A, 1, 3, 0,
		sys.AF, 1, 3, 0,
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

test( 'dporfs: basic_lower_3x3', function t() {
	var tc = findCase( 'basic_lower_3x3' );
	var sys = setupSystem( 'lower', [
		4, 2, 1,
		2, 5, 3,
		1, 3, 6
	], [ 1, 1, 1 ], 3, 1 );
	var info;

	info = dporfs( 'lower', 3, 1,
		sys.A, 1, 3, 0,
		sys.AF, 1, 3, 0,
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

test( 'dporfs: multi_rhs_3x3', function t() {
	var tc = findCase( 'multi_rhs_3x3' );
	var sys = setupSystem( 'upper', [
		4, 2, 1,
		2, 5, 3,
		1, 3, 6
	], [
		1, 2, 3,
		4, 5, 6
	], 3, 2 );
	var info;

	info = dporfs( 'upper', 3, 2,
		sys.A, 1, 3, 0,
		sys.AF, 1, 3, 0,
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
	assert.ok( sys.BERR[ 0 ] < 1e-10, 'BERR[0] is small' );
	assert.ok( sys.BERR[ 1 ] < 1e-10, 'BERR[1] is small' );
});

test( 'dporfs: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var A = new Float64Array( 1 );
	var info;

	info = dporfs( 'upper', 0, 1,
		A, 1, 1, 0,
		A, 1, 1, 0,
		A, 1, 1, 0,
		A, 1, 1, 0,
		FERR, 1, 0,
		BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assert.equal( FERR[ 0 ], 0.0, 'FERR = 0' );
	assert.equal( BERR[ 0 ], 0.0, 'BERR = 0' );
});

test( 'dporfs: nrhs_zero', function t() {
	var tc = findCase( 'nrhs_zero' );
	var A = new Float64Array( 9 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var info;

	info = dporfs( 'upper', 3, 0,
		A, 1, 3, 0,
		A, 1, 3, 0,
		A, 1, 3, 0,
		A, 1, 3, 0,
		FERR, 1, 0,
		BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
});

test( 'dporfs: illcond_5x5', function t() {
	var tc = findCase( 'illcond_5x5' );
	var sys = setupSystem( 'upper', [
		100, 10,  1,   0.1,  0.01,
		10,  100, 10,  1,    0.1,
		1,   10,  100, 10,   1,
		0.1, 1,   10,  100,  10,
		0.01, 0.1, 1,  10,   100
	], [ 1, 2, 3, 4, 5 ], 5, 1 );
	var info;

	info = dporfs( 'upper', 5, 1,
		sys.A, 1, 5, 0,
		sys.AF, 1, 5, 0,
		sys.B, 1, 5, 0,
		sys.X, 1, 5, 0,
		sys.FERR, 1, 0,
		sys.BERR, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( sys.X.subarray( 0, 5 ) ), tc.x, 1e-12, 'x' );
	assert.ok( sys.FERR[ 0 ] >= 0.0, 'FERR >= 0' );
	assert.ok( sys.BERR[ 0 ] >= 0.0, 'BERR >= 0' );
	assert.ok( sys.BERR[ 0 ] < 1e-10, 'BERR is small' );
});

test( 'dporfs: perturbed solution triggers iterative refinement', function t() {
	var A = new Float64Array([
		4, 2, 1,
		2, 5, 3,
		1, 3, 6
	]);
	var AF = new Float64Array( A );
	var B = new Float64Array([ 1, 1, 1 ]);
	var X = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var info;

	info = dpotrf( 'upper', 3, AF, 1, 3, 0 );
	assert.equal( info, 0, 'dpotrf should succeed' );

	X[ 0 ] = B[ 0 ]; X[ 1 ] = B[ 1 ]; X[ 2 ] = B[ 2 ];
	info = dpotrs( 'upper', 3, 1, AF, 1, 3, 0, X, 1, 3, 0 );
	assert.equal( info, 0, 'dpotrs should succeed' );

	// Perturb the solution to force refinement iterations
	X[ 0 ] += 1e-8;
	X[ 1 ] -= 1e-8;
	X[ 2 ] += 1e-8;

	info = dporfs( 'upper', 3, 1,
		A, 1, 3, 0,
		AF, 1, 3, 0,
		B, 1, 3, 0,
		X, 1, 3, 0,
		FERR, 1, 0,
		BERR, 1, 0
	);

	assert.equal( info, 0, 'info = 0' );
	var tc = findCase( 'basic_upper_3x3' );
	assertArrayClose( Array.from( X ), tc.x, 1e-12, 'x after refinement' );
	assert.ok( BERR[ 0 ] < 1e-10, 'BERR is small after refinement' );
});
