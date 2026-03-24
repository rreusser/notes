

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dposvx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dposvx.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Helper to call dposvx with standard workspace allocation.
*/
function callDposvx( fact, uplo, N, nrhs, A, AF, equed, s, B, X, FERR, BERR ) {
	var WORK = new Float64Array( Math.max( 3 * N, 1 ) );
	var IWORK = new Int32Array( Math.max( N, 1 ) );
	return dposvx( fact, uplo, N, nrhs, A, 1, N, 0, AF, 1, N, 0, equed, s, 1, 0, B, 1, N, 0, X, 1, N, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
}


// TESTS //

test( 'dposvx: fact_N_upper', function t() {
	var tc = findCase( 'fact_N_upper' );
	var A = new Float64Array([ 4.0, 1.0, 0.5, 1.0, 3.0, 1.0, 0.5, 1.0, 2.0 ]);
	var AF = new Float64Array( 9 );
	var s = new Float64Array( 3 );
	var B = new Float64Array([ 5.5, 5.0, 3.5 ]);
	var X = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var result = callDposvx( 'not-factored', 'upper', 3, 1, A, AF, 'none', s, B, X, FERR, BERR );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.equed, tc.equed, 'equed' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
	assertArrayClose( Array.from( X ), tc.x, 1e-10, 'x' );
	assertArrayClose( Array.from( FERR ), tc.ferr, 1e-2, 'ferr' );
	assertArrayClose( Array.from( BERR ), tc.berr, 1e-2, 'berr' );
});

test( 'dposvx: fact_N_lower', function t() {
	var tc = findCase( 'fact_N_lower' );
	var A = new Float64Array([ 4.0, 1.0, 0.5, 1.0, 3.0, 1.0, 0.5, 1.0, 2.0 ]);
	var AF = new Float64Array( 9 );
	var s = new Float64Array( 3 );
	var B = new Float64Array([ 5.5, 5.0, 3.5 ]);
	var X = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var result = callDposvx( 'not-factored', 'lower', 3, 1, A, AF, 'none', s, B, X, FERR, BERR );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.equed, tc.equed, 'equed' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
	assertArrayClose( Array.from( X ), tc.x, 1e-10, 'x' );
});

test( 'dposvx: fact_E', function t() {
	var tc = findCase( 'fact_E' );
	// Poorly scaled SPD matrix (upper triangle)
	var A = new Float64Array([ 100.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.1, 0.05, 0.01 ]);
	var AF = new Float64Array( 9 );
	var s = new Float64Array( 3 );
	var B = new Float64Array([ 101.1, 1.05, 0.16 ]);
	var X = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var result = callDposvx( 'equilibrate', 'upper', 3, 1, A, AF, 'none', s, B, X, FERR, BERR );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.equed, tc.equed, 'equed' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
	assertArrayClose( Array.from( X ), tc.x, 1e-10, 'x' );
	assertArrayClose( Array.from( s ), tc.s, 1e-10, 's' );
});

test( 'dposvx: fact_F', function t() {
	var tc = findCase( 'fact_F' );
	// First factor via FACT='not-factored'
	var A = new Float64Array([ 4.0, 1.0, 0.5, 1.0, 3.0, 1.0, 0.5, 1.0, 2.0 ]);
	var AF = new Float64Array( 9 );
	var s = new Float64Array( 3 );
	var B = new Float64Array([ 5.5, 5.0, 3.5 ]);
	var X = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	callDposvx( 'not-factored', 'upper', 3, 1, A, AF, 'none', s, B, X, FERR, BERR );

	// Now use FACT='factored' with the pre-factored AF
	A = new Float64Array([ 4.0, 1.0, 0.5, 1.0, 3.0, 1.0, 0.5, 1.0, 2.0 ]);
	B = new Float64Array([ 1.0, 2.0, 3.0 ]);
	X = new Float64Array( 3 );
	FERR = new Float64Array( 1 );
	BERR = new Float64Array( 1 );
	var result = callDposvx( 'factored', 'upper', 3, 1, A, AF, 'none', s, B, X, FERR, BERR );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.equed, tc.equed, 'equed' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
	assertArrayClose( Array.from( X ), tc.x, 1e-10, 'x' );
});

test( 'dposvx: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	var A = new Float64Array([ 1.0, 0.0, 0.0, 2.0, -1.0, 0.0, 3.0, 4.0, 5.0 ]);
	var AF = new Float64Array( 9 );
	var s = new Float64Array( 3 );
	var B = new Float64Array([ 1.0, 2.0, 3.0 ]);
	var X = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var result = callDposvx( 'not-factored', 'upper', 3, 1, A, AF, 'none', s, B, X, FERR, BERR );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.rcond, tc.rcond, 'rcond' );
});

test( 'dposvx: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var AF = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var X = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var result = callDposvx( 'not-factored', 'upper', 0, 1, A, AF, 'none', s, B, X, FERR, BERR );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dposvx: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var A = new Float64Array([ 4.0, 1.0, 0.5, 1.0, 3.0, 1.0, 0.5, 1.0, 2.0 ]);
	var AF = new Float64Array( 9 );
	var s = new Float64Array( 3 );
	var B = new Float64Array([ 5.5, 5.0, 3.5, 1.0, 2.0, 3.0 ]);
	var X = new Float64Array( 6 );
	var FERR = new Float64Array( 2 );
	var BERR = new Float64Array( 2 );
	var result = callDposvx( 'not-factored', 'upper', 3, 2, A, AF, 'none', s, B, X, FERR, BERR );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
	assertArrayClose( Array.from( X ), tc.x, 1e-10, 'x' );
	assertArrayClose( Array.from( FERR ), tc.ferr, 1e-2, 'ferr' );
	assertArrayClose( Array.from( BERR ), tc.berr, 1e-2, 'berr' );
});

test( 'dposvx: fact_F_with_equed_Y', function t() {
	// First equilibrate and factor via FACT='equilibrate'
	var A = new Float64Array([ 100.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.1, 0.05, 0.01 ]);
	var AF = new Float64Array( 9 );
	var s = new Float64Array( 3 );
	var B = new Float64Array([ 101.1, 1.05, 0.16 ]);
	var X = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var r1 = callDposvx( 'equilibrate', 'upper', 3, 1, A, AF, 'none', s, B, X, FERR, BERR );

	// Now use FACT='factored' with the equilibrated AF and S
	// Re-create the equilibrated A (since FACT='equilibrate' modified it)
	var A2 = new Float64Array([ 100.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.1, 0.05, 0.01 ]);
	var B2 = new Float64Array([ 101.1, 1.05, 0.16 ]);
	var X2 = new Float64Array( 3 );
	var FERR2 = new Float64Array( 1 );
	var BERR2 = new Float64Array( 1 );
	// Need to re-equilibrate A2 manually for the FACT='factored' call to compute norms correctly
	// Actually FACT='factored' with equed='yes' expects A to be the ORIGINAL (not equilibrated) matrix
	// and S to contain the scaling factors - Fortran reference uses A for computing ANORM
	var r2 = callDposvx( 'factored', 'upper', 3, 1, A2, AF, 'yes', s, B2, X2, FERR2, BERR2 );
	assert.equal( r2.info, 0, 'info' );
	assert.equal( r2.equed, 'yes', 'equed' );
	// Solution should be reasonable - verify via mathematical property
	assert.ok( r2.rcond > 0, 'rcond > 0' );
});

test( 'dposvx: fact_E_lower', function t() {
	var tc = findCase( 'fact_E_lower' );
	// Same poorly scaled SPD matrix but lower triangle
	var A = new Float64Array([ 100.0, 1.0, 0.1, 0.0, 1.0, 0.05, 0.0, 0.0, 0.01 ]);
	var AF = new Float64Array( 9 );
	var s = new Float64Array( 3 );
	var B = new Float64Array([ 101.1, 1.05, 0.16 ]);
	var X = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var result = callDposvx( 'equilibrate', 'lower', 3, 1, A, AF, 'none', s, B, X, FERR, BERR );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.equed, tc.equed, 'equed' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
	assertArrayClose( Array.from( X ), tc.x, 1e-10, 'x' );
});
