'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zgebrd = require( '../../zgebrd/lib/base.js' );
var zunmbr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zunmbr.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function extractRaw( C, count ) {
	var Cv = reinterpret( C, 0 );
	var result = [];
	var i;
	for ( i = 0; i < count; i++ ) {
		result.push( Cv[ i ] );
	}
	return result;
}

function setup4x3() {
	var LDA = 6;
	var A = new Complex128Array( LDA * 6 );
	var Av = reinterpret( A, 0 );
	Av[0]=1; Av[1]=0; Av[2]=2; Av[3]=1; Av[4]=0; Av[5]=-1; Av[6]=1; Av[7]=1;
	Av[2*LDA]=0; Av[2*LDA+1]=2; Av[2*LDA+2]=1; Av[2*LDA+3]=0; Av[2*LDA+4]=3; Av[2*LDA+5]=1; Av[2*LDA+6]=2; Av[2*LDA+7]=0;
	Av[4*LDA]=3; Av[4*LDA+1]=1; Av[4*LDA+2]=0; Av[4*LDA+3]=0; Av[4*LDA+4]=1; Av[4*LDA+5]=0; Av[4*LDA+6]=2; Av[4*LDA+7]=1;
	var TAUQ = new Complex128Array( 6 );
	var TAUP = new Complex128Array( 6 );
	var D = new Float64Array( 6 );
	var E = new Float64Array( 6 );
	var WORK = new Complex128Array( 200 );
	zgebrd( 4, 3, A, 1, LDA, 0, D, 1, 0, E, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 200 );
	return { A: A, TAUQ: TAUQ, TAUP: TAUP, LDA: LDA };
}

function setup3x5() {
	var LDA = 6;
	var A = new Complex128Array( LDA * 6 );
	var Av = reinterpret( A, 0 );
	Av[0]=1; Av[1]=0;
	Av[2*LDA]=2; Av[2*LDA+1]=1;
	Av[4*LDA]=0; Av[4*LDA+1]=0;
	Av[6*LDA]=1; Av[6*LDA+1]=1;
	Av[8*LDA]=3; Av[8*LDA+1]=0;
	Av[2]=0; Av[3]=2;
	Av[2*LDA+2]=1; Av[2*LDA+3]=0;
	Av[4*LDA+2]=3; Av[4*LDA+3]=1;
	Av[6*LDA+2]=2; Av[6*LDA+3]=0;
	Av[8*LDA+2]=1; Av[8*LDA+3]=1;
	Av[4]=3; Av[5]=1;
	Av[2*LDA+4]=0; Av[2*LDA+5]=0;
	Av[4*LDA+4]=1; Av[4*LDA+5]=0;
	Av[6*LDA+4]=2; Av[6*LDA+5]=1;
	Av[8*LDA+4]=0; Av[8*LDA+5]=2;
	var TAUQ = new Complex128Array( 6 );
	var TAUP = new Complex128Array( 6 );
	var D = new Float64Array( 6 );
	var E = new Float64Array( 6 );
	var WORK = new Complex128Array( 200 );
	zgebrd( 3, 5, A, 1, LDA, 0, D, 1, 0, E, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 200 );
	return { A: A, TAUQ: TAUQ, TAUP: TAUP, LDA: LDA };
}

function eye( n, ld ) {
	var C = new Complex128Array( ld * ld );
	var Cv = reinterpret( C, 0 );
	var i;
	for ( i = 0; i < n; i++ ) {
		Cv[ 2 * ( i + i * ld ) ] = 1.0;
	}
	return C;
}


// TESTS //

test( 'zunmbr: Q, left, no transpose', function t() {
	var tc = findCase( 'q_left_notrans' );
	var bd = setup4x3();
	var LDC = 6;
	var C = eye( 4, LDC );
	var WORK = new Complex128Array( 200 );
	var info = zunmbr( 'Q', 'L', 'N', 4, 4, 3, bd.A, 1, bd.LDA, 0, bd.TAUQ, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assert.equal( info, tc.info );
	assertArrayClose( extractRaw( C, tc.c.length ), tc.c, 1e-12, 'c' );
});

test( 'zunmbr: Q, left, conjugate transpose', function t() {
	var tc = findCase( 'q_left_conjtrans' );
	var bd = setup4x3();
	var LDC = 6;
	var C = eye( 4, LDC );
	var WORK = new Complex128Array( 200 );
	var info = zunmbr( 'Q', 'L', 'C', 4, 4, 3, bd.A, 1, bd.LDA, 0, bd.TAUQ, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assert.equal( info, tc.info );
	assertArrayClose( extractRaw( C, tc.c.length ), tc.c, 1e-12, 'c' );
});

test( 'zunmbr: P, right, no transpose', function t() {
	var tc = findCase( 'p_right_notrans' );
	var bd = setup3x5();
	var LDC = 6;
	var C = eye( 5, LDC );
	var WORK = new Complex128Array( 200 );
	var info = zunmbr( 'P', 'R', 'N', 5, 5, 3, bd.A, 1, bd.LDA, 0, bd.TAUP, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assert.equal( info, tc.info );
	assertArrayClose( extractRaw( C, tc.c.length ), tc.c, 1e-12, 'c' );
});

test( 'zunmbr: P, right, conjugate transpose', function t() {
	var tc = findCase( 'p_right_conjtrans' );
	var bd = setup3x5();
	var LDC = 6;
	var C = eye( 5, LDC );
	var WORK = new Complex128Array( 200 );
	var info = zunmbr( 'P', 'R', 'C', 5, 5, 3, bd.A, 1, bd.LDA, 0, bd.TAUP, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assert.equal( info, tc.info );
	assertArrayClose( extractRaw( C, tc.c.length ), tc.c, 1e-12, 'c' );
});

test( 'zunmbr: M=0 quick return', function t() {
	var A = new Complex128Array( 1 );
	var TAU = new Complex128Array( 1 );
	var C = new Complex128Array( 1 );
	var WORK = new Complex128Array( 1 );
	var info = zunmbr( 'Q', 'L', 'N', 0, 0, 0, A, 1, 1, 0, TAU, 1, 0, C, 1, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, 0 );
});

test( 'zunmbr: Q, right, no transpose', function t() {
	var tc = findCase( 'q_right_notrans' );
	var bd = setup4x3();
	var LDC = 6;
	var C = eye( 4, LDC );
	var WORK = new Complex128Array( 200 );
	var info = zunmbr( 'Q', 'R', 'N', 4, 4, 3, bd.A, 1, bd.LDA, 0, bd.TAUQ, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assert.equal( info, tc.info );
	assertArrayClose( extractRaw( C, tc.c.length ), tc.c, 1e-12, 'c' );
});

test( 'zunmbr: P, left, conjugate transpose', function t() {
	var tc = findCase( 'p_left_conjtrans' );
	var bd = setup3x5();
	var LDC = 6;
	var C = eye( 5, LDC );
	var WORK = new Complex128Array( 200 );
	var info = zunmbr( 'P', 'L', 'C', 5, 5, 3, bd.A, 1, bd.LDA, 0, bd.TAUP, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assert.equal( info, tc.info );
	assertArrayClose( extractRaw( C, tc.c.length ), tc.c, 1e-12, 'c' );
});
