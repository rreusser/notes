

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlasq3 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlasq3.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlasq3: main export is a function', function t() {
	assert.strictEqual( typeof dlasq3, 'function' );
});

test( 'dlasq3: n0_lt_i0 — immediate return', function t() {
	var tc = findCase( 'n0_lt_i0' );
	var z = new Float64Array( [ 4.0, 1.0, 4.0, 1.0 ] );
	var result = dlasq3( 2, 1, z, 1, 0, 0, 0.5, 0.0, 0.0, 4.0, 0, 0, 0, true, 0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assert.equal( result.pp, tc.pp );
	assertClose( result.sigma, tc.sigma, 1e-14, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-14, 'desig' );
	assert.equal( result.nfail, tc.nfail );
	assert.equal( result.iter, tc.iter );
	assert.equal( result.ndiv, tc.ndiv );
});

test( 'dlasq3: n0_eq_i0 — deflate single eigenvalue', function t() {
	var tc = findCase( 'n0_eq_i0' );
	var z = new Float64Array( [ 4.0, 1.0, 4.0, 1.0 ] );
	var result = dlasq3( 1, 1, z, 1, 0, 0, 0.5, 1.0, 0.0, 4.0, 0, 0, 0, true, 0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assertClose( z[ 0 ], tc.z1, 1e-14, 'z[0]' );
	assertClose( result.sigma, tc.sigma, 1e-14, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-14, 'desig' );
	assert.equal( result.iter, tc.iter );
});

test( 'dlasq3: n0_eq_i0_plus_1 — 2-eigenvalue deflation', function t() {
	var tc = findCase( 'n0_eq_i0_plus_1' );
	var z = new Float64Array( [
		4.0, 0.5, 4.0, 0.5,
		3.0, 0.0, 3.0, 0.0
	] );
	var result = dlasq3( 1, 2, z, 1, 0, 0, 0.5, 1.0, 0.0, 4.0, 0, 0, 0, true, 0, 0.5, 0.5, 3.0, 4.0, 0.5, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assertClose( result.sigma, tc.sigma, 1e-14, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-14, 'desig' );
	assertArrayClose( z, tc.z, 1e-14, 'z' );
	assert.equal( result.iter, tc.iter );
});

test( 'dlasq3: basic_n4_pp0 — main dqds path', function t() {
	var tc = findCase( 'basic_n4_pp0' );
	var z = new Float64Array( [
		4.0, 1.0, 4.0, 1.0,
		3.0, 0.5, 3.0, 0.5,
		2.0, 0.3, 2.0, 0.3,
		1.0, 0.0, 1.0, 0.0
	] );
	var result = dlasq3( 1, 4, z, 1, 0, 0, 1.0, 0.0, 0.0, 4.0, 0, 0, 0, true, 0, 1.0, 2.0, 1.0, 2.0, 3.0, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assert.equal( result.pp, tc.pp );
	assertClose( result.dmin, tc.dmin, 1e-12, 'dmin' );
	assertClose( result.sigma, tc.sigma, 1e-12, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-12, 'desig' );
	assertClose( result.qmax, tc.qmax, 1e-12, 'qmax' );
	assert.equal( result.nfail, tc.nfail );
	assert.equal( result.iter, tc.iter );
	assert.equal( result.ndiv, tc.ndiv );
	assert.equal( result.ttype, tc.ttype );
	assertClose( result.dmin1, tc.dmin1, 1e-12, 'dmin1' );
	assertClose( result.dmin2, tc.dmin2, 1e-12, 'dmin2' );
	assertClose( result.dn, tc.dn, 1e-12, 'dn' );
	assertClose( result.dn1, tc.dn1, 1e-12, 'dn1' );
	assertClose( result.dn2, tc.dn2, 1e-12, 'dn2' );
	assertClose( result.g, tc.g, 1e-12, 'g' );
	assertClose( result.tau, tc.tau, 1e-12, 'tau' );
	assertArrayClose( z, tc.z, 1e-12, 'z' );
});

test( 'dlasq3: basic_n5_pp0 — larger problem', function t() {
	var tc = findCase( 'basic_n5_pp0' );
	var z = new Float64Array( [
		4.0, 1.0, 4.0, 1.0,
		3.0, 0.5, 3.0, 0.5,
		5.0, 0.3, 5.0, 0.3,
		2.0, 0.2, 2.0, 0.2,
		1.5, 0.0, 1.5, 0.0
	] );
	var result = dlasq3( 1, 5, z, 1, 0, 0, 1.5, 0.0, 0.0, 5.0, 0, 0, 0, true, 0, 1.5, 2.0, 1.5, 2.0, 5.0, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assert.equal( result.pp, tc.pp );
	assertClose( result.dmin, tc.dmin, 1e-12, 'dmin' );
	assertClose( result.sigma, tc.sigma, 1e-12, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-12, 'desig' );
	assertClose( result.qmax, tc.qmax, 1e-12, 'qmax' );
	assert.equal( result.nfail, tc.nfail );
	assert.equal( result.iter, tc.iter );
	assert.equal( result.ndiv, tc.ndiv );
	assert.equal( result.ttype, tc.ttype );
	assertClose( result.dmin1, tc.dmin1, 1e-12, 'dmin1' );
	assertClose( result.dmin2, tc.dmin2, 1e-12, 'dmin2' );
	assertClose( result.dn, tc.dn, 1e-12, 'dn' );
	assertClose( result.dn1, tc.dn1, 1e-12, 'dn1' );
	assertClose( result.dn2, tc.dn2, 1e-12, 'dn2' );
	assertClose( result.g, tc.g, 1e-12, 'g' );
	assertClose( result.tau, tc.tau, 1e-12, 'tau' );
	assertArrayClose( z, tc.z, 1e-12, 'z' );
});

test( 'dlasq3: pp2_reset — pp=2 gets reset to 0', function t() {
	var tc = findCase( 'pp2_reset' );
	var z = new Float64Array( [
		4.0, 1.0, 4.0, 1.0,
		3.0, 0.5, 3.0, 0.5,
		2.0, 0.3, 2.0, 0.3,
		1.0, 0.0, 1.0, 0.0
	] );
	var result = dlasq3( 1, 4, z, 1, 0, 2, 1.0, 0.0, 0.0, 4.0, 0, 0, 0, true, 0, 1.0, 2.0, 1.0, 2.0, 3.0, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assert.equal( result.pp, tc.pp );
	assertClose( result.dmin, tc.dmin, 1e-12, 'dmin' );
	assertClose( result.sigma, tc.sigma, 1e-12, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-12, 'desig' );
	assert.equal( result.nfail, tc.nfail );
	assert.equal( result.iter, tc.iter );
	assert.equal( result.ndiv, tc.ndiv );
	assert.equal( result.ttype, tc.ttype );
	assertClose( result.tau, tc.tau, 1e-12, 'tau' );
	assertArrayClose( z, tc.z, 1e-12, 'z' );
});

test( 'dlasq3: dmin_negative_reversal — reversal path', function t() {
	var tc = findCase( 'dmin_negative_reversal' );
	var z = new Float64Array( [
		1.0, 0.5, 1.0, 0.5,
		2.0, 0.3, 2.0, 0.3,
		3.0, 0.2, 3.0, 0.2,
		4.0, 0.0, 4.0, 0.0
	] );
	var result = dlasq3( 1, 4, z, 1, 0, 0, -0.1, 0.0, 0.0, 4.0, 0, 0, 0, true, 0, 1.0, 2.0, 4.0, 3.0, 2.0, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assert.equal( result.pp, tc.pp );
	assertClose( result.dmin, tc.dmin, 1e-12, 'dmin' );
	assertClose( result.sigma, tc.sigma, 1e-12, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-12, 'desig' );
	assertClose( result.qmax, tc.qmax, 1e-12, 'qmax' );
	assert.equal( result.nfail, tc.nfail );
	assert.equal( result.iter, tc.iter );
	assert.equal( result.ndiv, tc.ndiv );
	assert.equal( result.ttype, tc.ttype );
	assertClose( result.dmin1, tc.dmin1, 1e-12, 'dmin1' );
	assertClose( result.dmin2, tc.dmin2, 1e-12, 'dmin2' );
	assertClose( result.dn, tc.dn, 1e-12, 'dn' );
	assertClose( result.dn1, tc.dn1, 1e-12, 'dn1' );
	assertClose( result.dn2, tc.dn2, 1e-12, 'dn2' );
	assertClose( result.tau, tc.tau, 1e-12, 'tau' );
	assertArrayClose( z, tc.z, 1e-12, 'z' );
});

test( 'dlasq3: non_ieee — non-IEEE arithmetic path', function t() {
	var tc = findCase( 'non_ieee' );
	var z = new Float64Array( [
		4.0, 1.0, 4.0, 1.0,
		3.0, 0.5, 3.0, 0.5,
		2.0, 0.3, 2.0, 0.3,
		1.0, 0.0, 1.0, 0.0
	] );
	var result = dlasq3( 1, 4, z, 1, 0, 0, 1.0, 0.0, 0.0, 4.0, 0, 0, 0, false, 0, 1.0, 2.0, 1.0, 2.0, 3.0, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assert.equal( result.pp, tc.pp );
	assertClose( result.dmin, tc.dmin, 1e-12, 'dmin' );
	assertClose( result.sigma, tc.sigma, 1e-12, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-12, 'desig' );
	assert.equal( result.nfail, tc.nfail );
	assert.equal( result.iter, tc.iter );
	assert.equal( result.ndiv, tc.ndiv );
	assert.equal( result.ttype, tc.ttype );
	assertClose( result.tau, tc.tau, 1e-12, 'tau' );
	assertArrayClose( z, tc.z, 1e-12, 'z' );
});

test( 'dlasq3: deflation_last — deflation of last eigenvalue', function t() {
	var tc = findCase( 'deflation_last' );
	var z = new Float64Array( [
		4.0, 1.0, 4.0, 1.0,
		3.0, 0.5, 3.0, 0.5,
		2.0, 0.3, 2.0, 0.3,
		5.0, 1.0e-30, 5.0, 1.0e-30,
		1.0, 0.0, 1.0, 0.0
	] );
	var result = dlasq3( 1, 5, z, 1, 0, 0, 1.0, 0.0, 0.0, 5.0, 0, 0, 0, true, 0, 1.0, 2.0, 1.0, 5.0, 2.0, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assert.equal( result.pp, tc.pp );
	assertClose( result.dmin, tc.dmin, 1e-12, 'dmin' );
	assertClose( result.sigma, tc.sigma, 1e-12, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-12, 'desig' );
	assert.equal( result.nfail, tc.nfail );
	assert.equal( result.iter, tc.iter );
	assert.equal( result.ndiv, tc.ndiv );
	assert.equal( result.ttype, tc.ttype );
	assertClose( result.tau, tc.tau, 1e-12, 'tau' );
	assertArrayClose( z, tc.z, 1e-12, 'z' );
});

test( 'dlasq3: n3_minimal — minimal 3-element problem', function t() {
	var tc = findCase( 'n3_minimal' );
	var z = new Float64Array( [
		4.0, 1.0, 4.0, 1.0,
		3.0, 0.5, 3.0, 0.5,
		2.0, 0.0, 2.0, 0.0
	] );
	var result = dlasq3( 1, 3, z, 1, 0, 0, 2.0, 0.0, 0.0, 4.0, 0, 0, 0, true, 0, 2.0, 3.0, 2.0, 3.0, 4.0, 0.0, 0.0 );
	assert.equal( result.n0, tc.n0 );
	assertClose( result.dmin, tc.dmin, 1e-12, 'dmin' );
	assertClose( result.sigma, tc.sigma, 1e-12, 'sigma' );
	assertClose( result.desig, tc.desig, 1e-12, 'desig' );
	assert.equal( result.nfail, tc.nfail );
	assert.equal( result.iter, tc.iter );
	assert.equal( result.ndiv, tc.ndiv );
	assert.equal( result.ttype, tc.ttype );
	assertClose( result.tau, tc.tau, 1e-12, 'tau' );
	assertArrayClose( z, tc.z, 1e-12, 'z' );
});

test( 'dlasq3: 2eig_swap — 2-eigenvalue deflation with swap', function t() {
	// n0=2, i0=1 enters label 40 directly. Z(nn-3)=3.0 > Z(nn-7)=1.0 triggers swap.
	var z = new Float64Array([
		1.0, 0.5, 1.0, 0.5,
		3.0, 0.0, 3.0, 0.0
	]);
	var result = dlasq3( 1, 2, z, 1, 0, 0, 0.5, 1.0, 0.0, 4.0, 0, 0, 0, true, 0, 0.5, 0.5, 3.0, 4.0, 0.5, 0.0, 0.0 );
	assert.equal( result.n0, 0 );
	assertClose( result.sigma, 1.0, 1e-14, 'sigma' );
});

test( 'dlasq3: 2eig_s_le_t — 2-eigenvalue deflation s<=t branch', function t() {
	// n0=2, Z(nn-3)=2.0 < Z(nn-7)=4.0 (no swap). s=0.8 <= t=1.25.
	var z = new Float64Array([
		4.0, 0.5, 4.0, 0.5,
		2.0, 0.0, 2.0, 0.0
	]);
	var result = dlasq3( 1, 2, z, 1, 0, 0, 0.5, 1.0, 0.0, 4.0, 0, 0, 0, true, 0, 0.5, 0.5, 2.0, 4.0, 0.5, 0.0, 0.0 );
	assert.equal( result.n0, 0 );
	assertClose( result.sigma, 1.0, 1e-14, 'sigma' );
});

test( 'dlasq3: tau_lt_sigma — tau < sigma path in sigma update', function t() {
	// With sigma=5.0, dlasq4 produces a small shift, tau < sigma.
	var z = new Float64Array([
		4.0, 1.0, 4.0, 1.0,
		3.0, 0.5, 3.0, 0.5,
		2.0, 0.3, 2.0, 0.3,
		1.0, 0.0, 1.0, 0.0
	]);
	var result = dlasq3( 1, 4, z, 1, 0, 0, 1.0, 5.0, 0.0, 4.0, 0, 0, 0, true, 0, 1.0, 2.0, 1.0, 2.0, 3.0, 0.0, 0.0 );
	assert.ok( result.tau < result.sigma, 'tau should be less than sigma' );
	assert.ok( result.dmin >= 0, 'dmin should be non-negative' );
});

test( 'dlasq3: ttype_lt_neg22 — tau set to zero after repeated failures', function t() {
	// Very small Z values with dmin=-1.0 forcing large initial tau.
	// dlasq5 fails repeatedly, driving ttype below -22.
	var z = new Float64Array([
		0.01, 0.01, 0.01, 0.01,
		0.01, 0.01, 0.01, 0.01,
		0.01, 0.01, 0.01, 0.01,
		0.01, 0.0,  0.01, 0.0
	]);
	var result = dlasq3( 1, 4, z, 1, 0, 0, -1.0, 0.0, 0.0, 0.01, 0, 0, 0, true, 0, 0.01, 0.01, 0.01, 0.01, 0.01, 0.0, 0.0 );
	assert.ok( result.nfail >= 3, 'should have at least 3 failures' );
	assert.ok( result.dmin >= 0, 'dmin should converge to non-negative' );
	assertClose( result.tau, 0.0, 1e-14, 'tau should be zero after safe fallback' );
});
