'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarrk = require( './../lib/dlarrk.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarrk.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	});
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// TESTS //

test( 'dlarrk is a function', function t() {
	assert.strictEqual( typeof dlarrk, 'function', 'is a function' );
});

test( 'dlarrk has expected arity', function t() {
	assert.strictEqual( dlarrk.length, 8, 'has expected arity' );
});

test( 'dlarrk throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarrk( -1, 1, 0.0, 1.0, new Float64Array( 1 ), new Float64Array( 1 ), 1e-16, 1e-12 );
	}, RangeError );
});

test( 'dlarrk: n0_quick (quick return for N=0)', function t() {
	var tc = findCase( 'n0_quick' );
	var d = new Float64Array( [ 0.0 ] );
	var e2 = new Float64Array( [ 0.0 ] );
	var r = dlarrk( 0, 1, -1.0, 1.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-14, 'w' );
	assertClose( r.werr, tc.werr, 1e-14, 'werr' );
});

test( 'dlarrk: n2_iw1 matches fixture tightly', function t() {
	var tc = findCase( 'n2_iw1' );
	var d = new Float64Array( [ 1.0, 4.0 ] );
	var e2 = new Float64Array( [ 1.0 ] );
	var r = dlarrk( 2, 1, 0.0, 5.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	// Also compare against the analytic eigenvalue of [[1,1],[1,4]]:
	assert.ok( Math.abs( r.w - ( ( 5.0 - Math.sqrt( 13.0 ) ) / 2.0 ) ) < 1e-10 );
});

test( 'dlarrk: n2_iw2 matches fixture tightly', function t() {
	var tc = findCase( 'n2_iw2' );
	var d = new Float64Array( [ 1.0, 4.0 ] );
	var e2 = new Float64Array( [ 1.0 ] );
	var r = dlarrk( 2, 2, 0.0, 5.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assert.ok( Math.abs( r.w - ( ( 5.0 + Math.sqrt( 13.0 ) ) / 2.0 ) ) < 1e-10 );
});

test( 'dlarrk: n3_diag_iw2 matches fixture', function t() {
	var tc = findCase( 'n3_diag_iw2' );
	var d = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var e2 = new Float64Array( [ 0.0, 0.0 ] );
	var r = dlarrk( 3, 2, 0.0, 4.0, d, e2, 1e-300, 1e-12 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
});

test( 'dlarrk: n1_loose tolerance matches fixture', function t() {
	var tc = findCase( 'n1_loose' );
	var d = new Float64Array( [ 1.0 ] );
	var e2 = new Float64Array( [ 0.0 ] );
	var r = dlarrk( 1, 1, -1.0, 3.0, d, e2, 1e-16, 1e-2 );
	assert.equal( r.info, tc.info );
	assertClose( r.w, tc.w, 1e-10, 'w' );
	assertClose( r.werr, tc.werr, 1e-10, 'werr' );
});
