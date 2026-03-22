

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlasr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlasr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		var relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}

/**
* Returns a 4x4 matrix A = [1 5 9 13; 2 6 10 14; 3 7 11 15; 4 8 12 16]
* in column-major flat array.
*/
function initMatrix() {
	return new Float64Array([
		1, 2, 3, 4,
		5, 6, 7, 8,
		9, 10, 11, 12,
		13, 14, 15, 16
	]);
}

/**
* Returns the c and s arrays used in all 12 combination tests.
*/
function initCS() {
	return {
		c: new Float64Array([ 0.6, 0.8, 0.5 ]),
		s: new Float64Array([ 0.8, 0.6, 0.866025403784439 ])
	};
}

/**
* Helper to run a single test case against fixture.
*/
function runCase( side, pivot, direct, M, N, caseName ) {
	var tc = findCase( caseName );
	var cs = initCS();
	var A = initMatrix();
	dlasr( side, pivot, direct, M, N, cs.c, 1, 0, cs.s, 1, 0, A, 1, M, 0 );
	assertArrayClose( A, tc.A, 1e-14, caseName + ' A' );
}


// TESTS //

test( 'dlasr: m_zero (quick return)', function t() {
	var tc = findCase( 'm_zero' );
	var A = initMatrix();
	var expected = new Float64Array( A );
	var cs = initCS();
	dlasr( 'L', 'V', 'F', 0, 4, cs.c, 1, 0, cs.s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'm_zero A' );
});

test( 'dlasr: n_zero (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = initMatrix();
	var expected = new Float64Array( A );
	var cs = initCS();
	dlasr( 'L', 'V', 'F', 4, 0, cs.c, 1, 0, cs.s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'n_zero A' );
});

test( 'dlasr: SIDE=L, PIVOT=V, DIRECT=F', function t() {
	runCase( 'L', 'V', 'F', 4, 4, 'L_V_F' );
});

test( 'dlasr: SIDE=L, PIVOT=V, DIRECT=B', function t() {
	runCase( 'L', 'V', 'B', 4, 4, 'L_V_B' );
});

test( 'dlasr: SIDE=L, PIVOT=T, DIRECT=F', function t() {
	runCase( 'L', 'T', 'F', 4, 4, 'L_T_F' );
});

test( 'dlasr: SIDE=L, PIVOT=T, DIRECT=B', function t() {
	runCase( 'L', 'T', 'B', 4, 4, 'L_T_B' );
});

test( 'dlasr: SIDE=L, PIVOT=B, DIRECT=F', function t() {
	runCase( 'L', 'B', 'F', 4, 4, 'L_B_F' );
});

test( 'dlasr: SIDE=L, PIVOT=B, DIRECT=B', function t() {
	runCase( 'L', 'B', 'B', 4, 4, 'L_B_B' );
});

test( 'dlasr: SIDE=R, PIVOT=V, DIRECT=F', function t() {
	runCase( 'R', 'V', 'F', 4, 4, 'R_V_F' );
});

test( 'dlasr: SIDE=R, PIVOT=V, DIRECT=B', function t() {
	runCase( 'R', 'V', 'B', 4, 4, 'R_V_B' );
});

test( 'dlasr: SIDE=R, PIVOT=T, DIRECT=F', function t() {
	runCase( 'R', 'T', 'F', 4, 4, 'R_T_F' );
});

test( 'dlasr: SIDE=R, PIVOT=T, DIRECT=B', function t() {
	runCase( 'R', 'T', 'B', 4, 4, 'R_T_B' );
});

test( 'dlasr: SIDE=R, PIVOT=B, DIRECT=F', function t() {
	runCase( 'R', 'B', 'F', 4, 4, 'R_B_F' );
});

test( 'dlasr: SIDE=R, PIVOT=B, DIRECT=B', function t() {
	runCase( 'R', 'B', 'B', 4, 4, 'R_B_B' );
});

test( 'dlasr: returns A', function t() {
	var cs = initCS();
	var A = initMatrix();
	var result = dlasr( 'L', 'V', 'F', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A, 1, 4, 0 );
	assert.equal( result, A );
});

test( 'dlasr: lowercase parameters match uppercase', function t() {
	var A1;
	var A2;
	var cs;

	// Test lowercase 'l', 'v', 'f'
	cs = initCS();
	A1 = initMatrix();
	A2 = initMatrix();
	dlasr( 'L', 'V', 'F', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A1, 1, 4, 0 );
	dlasr( 'l', 'v', 'f', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A2, 1, 4, 0 );
	assertArrayClose( A2, A1, 1e-14, 'l_v_f lowercase' );

	// Test lowercase 'l', 'v', 'b'
	cs = initCS();
	A1 = initMatrix();
	A2 = initMatrix();
	dlasr( 'L', 'V', 'B', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A1, 1, 4, 0 );
	dlasr( 'l', 'v', 'b', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A2, 1, 4, 0 );
	assertArrayClose( A2, A1, 1e-14, 'l_v_b lowercase' );

	// Test lowercase 'l', 't', 'f'
	cs = initCS();
	A1 = initMatrix();
	A2 = initMatrix();
	dlasr( 'L', 'T', 'F', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A1, 1, 4, 0 );
	dlasr( 'l', 't', 'f', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A2, 1, 4, 0 );
	assertArrayClose( A2, A1, 1e-14, 'l_t_f lowercase' );

	// Test lowercase 'l', 'b', 'f'
	cs = initCS();
	A1 = initMatrix();
	A2 = initMatrix();
	dlasr( 'L', 'B', 'F', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A1, 1, 4, 0 );
	dlasr( 'l', 'b', 'f', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A2, 1, 4, 0 );
	assertArrayClose( A2, A1, 1e-14, 'l_b_f lowercase' );

	// Test lowercase 'r', 'v', 'f'
	cs = initCS();
	A1 = initMatrix();
	A2 = initMatrix();
	dlasr( 'R', 'V', 'F', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A1, 1, 4, 0 );
	dlasr( 'r', 'v', 'f', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A2, 1, 4, 0 );
	assertArrayClose( A2, A1, 1e-14, 'r_v_f lowercase' );

	// Test lowercase 'r', 't', 'f'
	cs = initCS();
	A1 = initMatrix();
	A2 = initMatrix();
	dlasr( 'R', 'T', 'F', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A1, 1, 4, 0 );
	dlasr( 'r', 't', 'f', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A2, 1, 4, 0 );
	assertArrayClose( A2, A1, 1e-14, 'r_t_f lowercase' );

	// Test lowercase 'r', 'b', 'f'
	cs = initCS();
	A1 = initMatrix();
	A2 = initMatrix();
	dlasr( 'R', 'B', 'F', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A1, 1, 4, 0 );
	dlasr( 'r', 'b', 'f', 4, 4, cs.c, 1, 0, cs.s, 1, 0, A2, 1, 4, 0 );
	assertArrayClose( A2, A1, 1e-14, 'r_b_f lowercase' );
});

test( 'dlasr: identity rotation (c=1, s=0) skips computation', function t() {
	var A;
	var expected;
	var c;
	var s;

	// When all c=1 and s=0, the matrix should not change
	c = new Float64Array([ 1.0, 1.0, 1.0 ]);
	s = new Float64Array([ 0.0, 0.0, 0.0 ]);
	A = initMatrix();
	expected = new Float64Array( A );
	dlasr( 'L', 'V', 'F', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'L_V_F identity' );

	A = initMatrix();
	dlasr( 'L', 'T', 'F', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'L_T_F identity' );

	A = initMatrix();
	dlasr( 'L', 'B', 'F', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'L_B_F identity' );

	A = initMatrix();
	dlasr( 'R', 'V', 'F', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'R_V_F identity' );

	A = initMatrix();
	dlasr( 'R', 'T', 'F', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'R_T_F identity' );

	A = initMatrix();
	dlasr( 'R', 'B', 'F', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'R_B_F identity' );

	// Also test backward direction
	A = initMatrix();
	dlasr( 'L', 'V', 'B', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'L_V_B identity' );

	A = initMatrix();
	dlasr( 'L', 'T', 'B', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'L_T_B identity' );

	A = initMatrix();
	dlasr( 'L', 'B', 'B', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'L_B_B identity' );

	A = initMatrix();
	dlasr( 'R', 'V', 'B', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'R_V_B identity' );

	A = initMatrix();
	dlasr( 'R', 'T', 'B', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'R_T_B identity' );

	A = initMatrix();
	dlasr( 'R', 'B', 'B', 4, 4, c, 1, 0, s, 1, 0, A, 1, 4, 0 );
	assertArrayClose( A, expected, 1e-14, 'R_B_B identity' );
});
