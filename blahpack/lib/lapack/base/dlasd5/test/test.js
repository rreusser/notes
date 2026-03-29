

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var path = require( 'path' );
var dlasd5 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlasd5.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Runs dlasd5 with fixture data and compares outputs.
*
* @private
* @param {string} name - test case name from fixture
* @param {integer} i - eigenvalue index (1 or 2)
*/
function runTest( name, i ) {
	var dsigma;
	var delta;
	var work;
	var tc;
	var d;
	var z;

	tc = findCase( name );
	d = new Float64Array( tc.D );
	z = new Float64Array( tc.Z );
	delta = new Float64Array( 2 );
	dsigma = new Float64Array( 1 );
	work = new Float64Array( 2 );

	dlasd5( i, d, 1, 0, z, 1, 0, delta, 1, 0, tc.rho, dsigma, work, 1, 0 );

	assertClose( dsigma[ 0 ], tc.dsigma, 1e-14, 'dsigma' );
	assertArrayClose( delta, tc.delta, 1e-14, 'delta' );
	assertArrayClose( work, tc.work, 1e-14, 'work' );
}


// TESTS //

test( 'dlasd5 is a function', function t() {
	assert.equal( typeof dlasd5, 'function' );
});

test( 'dlasd5: I=1, W>0 branch (basic case)', function t() {
	runTest( 'i1_w_positive', 1 );
});

test( 'dlasd5: I=1, W<=0, B>0 branch', function t() {
	runTest( 'i1_w_nonpositive', 1 );
});

test( 'dlasd5: I=2, B>0 branch', function t() {
	runTest( 'i2_b_positive', 2 );
});

test( 'dlasd5: I=2, B<=0 branch', function t() {
	runTest( 'i2_b_nonpositive', 2 );
});

test( 'dlasd5: I=1, W<=0, B<=0 branch', function t() {
	runTest( 'i1_w_nonpos_b_nonpos', 1 );
});

test( 'dlasd5: D(1)=0 edge case, I=1', function t() {
	runTest( 'd1_zero_i1', 1 );
});

test( 'dlasd5: D(1)=0 edge case, I=2', function t() {
	runTest( 'd1_zero_i2', 2 );
});

test( 'dlasd5: large rho, I=1', function t() {
	runTest( 'large_rho_i1', 1 );
});

test( 'dlasd5: large rho, I=2', function t() {
	runTest( 'large_rho_i2', 2 );
});

test( 'dlasd5: supports non-unit strides', function t() {
	var dsigma;
	var delta;
	var work;
	var tc;
	var d;
	var z;

	tc = findCase( 'i1_w_positive' );

	// Store D and Z with stride 2
	d = new Float64Array( [ tc.D[ 0 ], 0.0, tc.D[ 1 ] ] );
	z = new Float64Array( [ tc.Z[ 0 ], 0.0, tc.Z[ 1 ] ] );
	delta = new Float64Array( 3 );
	dsigma = new Float64Array( 1 );
	work = new Float64Array( 3 );

	dlasd5( 1, d, 2, 0, z, 2, 0, delta, 2, 0, tc.rho, dsigma, work, 2, 0 );

	assertClose( dsigma[ 0 ], tc.dsigma, 1e-14, 'dsigma' );
	assertClose( delta[ 0 ], tc.delta[ 0 ], 1e-14, 'delta[0]' );
	assertClose( delta[ 2 ], tc.delta[ 1 ], 1e-14, 'delta[1]' );
	assertClose( work[ 0 ], tc.work[ 0 ], 1e-14, 'work[0]' );
	assertClose( work[ 2 ], tc.work[ 1 ], 1e-14, 'work[1]' );
});

test( 'dlasd5: supports offset parameters', function t() {
	var dsigma;
	var delta;
	var work;
	var tc;
	var d;
	var z;

	tc = findCase( 'i2_b_positive' );

	// Store D and Z starting at offset 1
	d = new Float64Array( [ 0.0, tc.D[ 0 ], tc.D[ 1 ] ] );
	z = new Float64Array( [ 0.0, tc.Z[ 0 ], tc.Z[ 1 ] ] );
	delta = new Float64Array( 4 );
	dsigma = new Float64Array( 1 );
	work = new Float64Array( 4 );

	dlasd5( 2, d, 1, 1, z, 1, 1, delta, 1, 2, tc.rho, dsigma, work, 1, 2 );

	assertClose( dsigma[ 0 ], tc.dsigma, 1e-14, 'dsigma' );
	assertClose( delta[ 2 ], tc.delta[ 0 ], 1e-14, 'delta[0]' );
	assertClose( delta[ 3 ], tc.delta[ 1 ], 1e-14, 'delta[1]' );
	assertClose( work[ 2 ], tc.work[ 0 ], 1e-14, 'work[0]' );
	assertClose( work[ 3 ], tc.work[ 1 ], 1e-14, 'work[1]' );
});
