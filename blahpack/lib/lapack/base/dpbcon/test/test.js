

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dpbtrf = require( './../../dpbtrf/lib/base.js' );
var dpbcon = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpbcon.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

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

	// SPD matrix: [4 1 0 0; 1 5 1 0; 0 1 6 2; 0 0 2 7]
	// Upper banded (KD=1, LDAB=2): row 0=superdiag, row 1=main
	ab = bandedMatrix( 2, n, [
		[1, 0, 4.0],
		[0, 1, 1.0], [1, 1, 5.0],
		[0, 2, 1.0], [1, 2, 6.0],
		[0, 3, 2.0], [1, 3, 7.0]
	]);
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );

	info = dpbtrf( 'upper', n, 1, ab, 1, 2, 0 );
	assert.equal( info, 0, 'dpbtrf info' );

	info = dpbcon( 'upper', n, 1, ab, 1, 2, 0, tc.anorm, rcond, work, 1, 0, iwork, 1, 0 );
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

	// Same SPD matrix, lower banded (KD=1, LDAB=2): row 0=main, row 1=subdiag
	ab = bandedMatrix( 2, n, [
		[0, 0, 4.0], [1, 0, 1.0],
		[0, 1, 5.0], [1, 1, 1.0],
		[0, 2, 6.0], [1, 2, 2.0],
		[0, 3, 7.0]
	]);
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );

	info = dpbtrf( 'lower', n, 1, ab, 1, 2, 0 );
	assert.equal( info, 0, 'dpbtrf info' );

	info = dpbcon( 'lower', n, 1, ab, 1, 2, 0, tc.anorm, rcond, work, 1, 0, iwork, 1, 0 );
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

	// SPD: [10 2 1 0; 2 10 3 1; 1 3 10 2; 0 1 2 10]
	// Upper banded (KD=2, LDAB=3): row 0=super-2, row 1=super-1, row 2=main
	ab = bandedMatrix( 3, n, [
		[2, 0, 10.0],
		[1, 1, 2.0], [2, 1, 10.0],
		[0, 2, 1.0], [1, 2, 3.0], [2, 2, 10.0],
		[0, 3, 1.0], [1, 3, 2.0], [2, 3, 10.0]
	]);
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );

	info = dpbtrf( 'upper', n, 2, ab, 1, 3, 0 );
	assert.equal( info, 0, 'dpbtrf info' );

	info = dpbcon( 'upper', n, 2, ab, 1, 3, 0, tc.anorm, rcond, work, 1, 0, iwork, 1, 0 );
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

	info = dpbcon( 'upper', 0, 0, ab, 1, 1, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 );
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

	info = dpbcon( 'upper', 1, 0, ab, 1, 1, 0, 4.0, rcond, work, 1, 0, iwork, 1, 0 );
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
		[0, 1, 1.0], [1, 1, 5.0],
		[0, 2, 1.0], [1, 2, 6.0],
		[0, 3, 2.0], [1, 3, 7.0]
	]);
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );

	dpbtrf( 'upper', n, 1, ab, 1, 2, 0 );

	info = dpbcon( 'upper', n, 1, ab, 1, 2, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-14, 'rcond' );
});
