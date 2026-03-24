'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dptrfs = require( './../lib/base.js' );
var dpttrf = require( '../../dpttrf/lib/base.js' );
var dpttrs = require( '../../dpttrs/lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dptrfs.jsonl' ), 'utf8' ).trim().split( '\n' );
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


// TESTS //

test( 'dptrfs: basic_5x5', function t() {
	var tc = findCase( 'basic_5x5' );
	var d = new Float64Array( [ 4, 3, 2, 3, 4 ] );
	var e = new Float64Array( [ 2, -1.5, 0.5, -0.75 ] );
	var df = new Float64Array( d );
	var ef = new Float64Array( e );
	var b = new Float64Array( [ 8, 3.5, 5, 9.75, 17 ] );
	var x = new Float64Array( b );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 10 );
	var info;

	dpttrf( 5, df, 1, 0, ef, 1, 0 );
	dpttrs( 5, 1, df, 1, 0, ef, 1, 0, x, 1, 5, 0 );
	info = dptrfs( 5, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 5, 0, x, 1, 5, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0 );

	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( ferr ), tc.ferr, 0.5, 'ferr' );
	assertArrayClose( Array.from( berr ), tc.berr, 1e-14, 'berr' );
});

test( 'dptrfs: multi_rhs_2', function t() {
	var tc = findCase( 'multi_rhs_2' );
	var d = new Float64Array( [ 4, 3, 2, 3, 4 ] );
	var e = new Float64Array( [ 2, -1.5, 0.5, -0.75 ] );
	var df = new Float64Array( d );
	var ef = new Float64Array( e );
	// Column-major: col1=[8,3.5,5,9.75,17], col2=[28,17.5,1,6.75,2.5]
	var b = new Float64Array( [ 8, 3.5, 5, 9.75, 17, 28, 17.5, 1, 6.75, 2.5 ] );
	var x = new Float64Array( b );
	var ferr = new Float64Array( 2 );
	var berr = new Float64Array( 2 );
	var work = new Float64Array( 10 );
	var info;

	dpttrf( 5, df, 1, 0, ef, 1, 0 );
	dpttrs( 5, 2, df, 1, 0, ef, 1, 0, x, 1, 5, 0 );
	info = dptrfs( 5, 2, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 5, 0, x, 1, 5, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0 );

	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( ferr ), tc.ferr, 0.5, 'ferr' );
	assertArrayClose( Array.from( berr ), tc.berr, 1e-10, 'berr' );
});

test( 'dptrfs: n_eq_1', function t() {
	var tc = findCase( 'n_eq_1' );
	var d = new Float64Array( [ 5 ] );
	var e = new Float64Array( 0 );
	var df = new Float64Array( d );
	var ef = new Float64Array( 0 );
	var b = new Float64Array( [ 15 ] );
	var x = new Float64Array( b );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 2 );
	var info;

	dpttrf( 1, df, 1, 0, ef, 1, 0 );
	dpttrs( 1, 1, df, 1, 0, ef, 1, 0, x, 1, 1, 0 );
	info = dptrfs( 1, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 1, 0, x, 1, 1, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0 );

	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	assertArrayClose( Array.from( ferr ), tc.ferr, 0.5, 'ferr' );
	assertArrayClose( Array.from( berr ), tc.berr, 1e-14, 'berr' );
});

test( 'dptrfs: n_eq_0', function t() {
	var d = new Float64Array( 0 );
	var e = new Float64Array( 0 );
	var df = new Float64Array( 0 );
	var ef = new Float64Array( 0 );
	var b = new Float64Array( 0 );
	var x = new Float64Array( 0 );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 0 );
	var info;

	info = dptrfs( 0, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 1, 0, x, 1, 1, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0 );

	assert.equal( info, 0, 'info' );
	assert.equal( ferr[ 0 ], 0.0, 'ferr zeroed' );
	assert.equal( berr[ 0 ], 0.0, 'berr zeroed' );
});

test( 'dptrfs: nrhs_eq_0', function t() {
	var d = new Float64Array( [ 4, 3, 2, 3, 4 ] );
	var e = new Float64Array( [ 2, -1.5, 0.5, -0.75 ] );
	var df = new Float64Array( d );
	var ef = new Float64Array( e );
	var b = new Float64Array( 5 );
	var x = new Float64Array( 5 );
	var ferr = new Float64Array( 0 );
	var berr = new Float64Array( 0 );
	var work = new Float64Array( 10 );
	var info;

	info = dptrfs( 5, 0, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 5, 0, x, 1, 5, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0 );

	assert.equal( info, 0, 'info' );
});

test( 'dptrfs: perturbed', function t() {
	var tc = findCase( 'perturbed' );
	var d = new Float64Array( [ 4, 3, 2, 3, 4 ] );
	var e = new Float64Array( [ 2, -1.5, 0.5, -0.75 ] );
	var df = new Float64Array( d );
	var ef = new Float64Array( e );
	var b = new Float64Array( [ 8, 3.5, 5, 9.75, 17 ] );
	var x = new Float64Array( b );
	var ferr = new Float64Array( 1 );
	var berr = new Float64Array( 1 );
	var work = new Float64Array( 10 );
	var info;

	dpttrf( 5, df, 1, 0, ef, 1, 0 );
	dpttrs( 5, 1, df, 1, 0, ef, 1, 0, x, 1, 5, 0 );

	// Add perturbation:
	x[ 0 ] += 1e-10;
	x[ 2 ] -= 1e-10;
	x[ 4 ] += 1e-10;

	info = dptrfs( 5, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 5, 0, x, 1, 5, 0, ferr, 1, 0, berr, 1, 0, work, 1, 0 );

	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( x ), tc.x, 1e-14, 'x' );
	// BERR should be small after refinement
	assert.ok( berr[ 0 ] < 1e-14, 'berr is small' );
});
